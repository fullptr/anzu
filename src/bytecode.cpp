#include "bytecode.hpp"
#include "object.hpp"
#include "functions.hpp"
#include "utility/scope_timer.hpp"
#include "utility/print.hpp"
#include "utility/memory.hpp"

#include <concepts>

namespace anzu {
namespace {

template <typename ...Args>
auto runtime_assert(bool condition, std::string_view msg, Args&&... args)
{
    if (!condition) {
        anzu::print(msg, std::forward<Args>(args)...);
        std::exit(1);
    }
}


template <typename ...Args>
[[noreturn]] auto runtime_error(std::string_view message, Args&&... args)
{
    const auto msg = std::format(message, std::forward<Args>(args)...);
    print("Runtime assertion failed! {}\n", msg);
    std::exit(1);
}

template <typename Type, template <typename> typename Op>
auto unary_op(bytecode_context& ctx) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto obj = pop_value<Type>(ctx.stack);
    push_value(ctx.stack, op(obj));
}

template <typename Type, template <typename> typename Op>
auto binary_op(bytecode_context& ctx) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto rhs = pop_value<Type>(ctx.stack);
    const auto lhs = pop_value<Type>(ctx.stack);
    push_value(ctx.stack, op(lhs, rhs));
}

template <typename T>
requires std::integral<T> || std::floating_point<T>
auto read(const bytecode_program& prog, std::size_t& ptr) -> T
{
    auto ret = T{0};
    std::memcpy(&ret, &prog.code[ptr], sizeof(T));
    ptr += sizeof(T);
    return ret;
}

auto to_byte(op2 opcode) -> std::byte
{
    return static_cast<std::byte>(opcode);
}

template <typename T>
auto to_bytes(T t) -> std::array<std::byte, sizeof(T)>
{
    return std::bit_cast<std::array<std::byte, sizeof(T)>>(t);
}

auto apply_op(const bytecode_program& prog, bytecode_context& ctx) -> void
{
    const auto op_code = static_cast<op2>(prog.code[ctx.prog_ptr++]);
    switch (op_code) {
        // TODO: Pushing literals can just be memcpy's without casting, because we're
        // going from bytes to bytes
        case op2::push_literal_i32: {
            const auto value = read<std::int32_t>(prog, ctx.prog_ptr);
            push_value(ctx.stack, value);
        } break;
        case op2::push_literal_i64: {
            const auto value = read<std::int64_t>(prog, ctx.prog_ptr);
            push_value(ctx.stack, value);
        } break;
        case op2::push_literal_u64: {
            const auto value = read<std::uint64_t>(prog, ctx.prog_ptr);
            push_value(ctx.stack, value);
        } break;
        case op2::push_literal_f64: {
            const auto value = read<double>(prog, ctx.prog_ptr);
            push_value(ctx.stack, value);
        } break;
        case op2::push_literal_char: {
            const auto value = read<char>(prog, ctx.prog_ptr);
            push_value(ctx.stack, value);
        } break;
        case op2::push_literal_bool: {
            const auto value = read<bool>(prog, ctx.prog_ptr);
            push_value(ctx.stack, value);
        } break;
        case op2::push_literal_null: {
            push_value(ctx.stack, std::byte{0});
        } break;
        case op2::push_global_addr: {
            const auto pos = read<std::uint64_t>(prog, ctx.prog_ptr);
            push_value(ctx.stack, pos);
        } break;
        case op2::push_local_addr: {
            const auto offset = read<std::uint64_t>(prog, ctx.prog_ptr);
            push_value(ctx.stack, ctx.base_ptr + offset);
        } break;
        case op2::load: {
            const auto size = read<std::uint64_t>(prog, ctx.prog_ptr);

            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            if (is_heap_ptr(ptr)) {
                const auto heap_ptr = unset_heap_bit(ptr);
                for (std::size_t i = 0; i != size; ++i) {
                    ctx.stack.push_back(ctx.heap[heap_ptr + i]);
                }
            }
            else if (is_rom_ptr(ptr)) {
                const auto rom_ptr = unset_rom_bit(ptr);
                for (std::size_t i = 0; i != size; ++i) {
                    ctx.stack.push_back(ctx.rom[rom_ptr + i]);
                }
            }
            else {
                for (std::size_t i = 0; i != size; ++i) {
                    ctx.stack.push_back(ctx.stack[ptr + i]);
                }
            }
        } break;
        case op2::save: {
            const auto size = read<std::uint64_t>(prog, ctx.prog_ptr);

            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            if (is_heap_ptr(ptr)) {
                const auto heap_ptr = unset_heap_bit(ptr);
                std::memcpy(&ctx.heap[heap_ptr], &ctx.stack[ctx.stack.size() - size], size);
                ctx.stack.resize(ctx.stack.size() - size);
            }
            else if (is_rom_ptr(ptr)) {
                runtime_error("cannot assign into read only memory");
            }
            else {
                runtime_assert(ptr + size <= ctx.stack.size(), "tried to access invalid memory address {}", ptr);
                if (ptr + size < ctx.stack.size()) {
                    std::memcpy(&ctx.stack[ptr], &ctx.stack[ctx.stack.size() - size], size);
                    ctx.stack.resize(ctx.stack.size() - size);
                }
            }
        } break;
        case op2::pop: {
            const auto size = read<std::uint64_t>(prog, ctx.prog_ptr);
            ctx.stack.resize(ctx.stack.size() - size);
        } break;
        case op2::alloc_span: {
            const auto type_size = read<std::uint64_t>(prog, ctx.prog_ptr);
            const auto count = pop_value<std::uint64_t>(ctx.stack);
            const auto ptr = ctx.allocator.allocate(count * type_size);
            push_value(ctx.stack, set_heap_bit(ptr));
        } break;
        case op2::dealloc_span: {
            const auto type_size = read<std::uint64_t>(prog, ctx.prog_ptr);
            const auto count = pop_value<std::uint64_t>(ctx.stack);
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            runtime_assert(is_heap_ptr(ptr), "cannot delete a span to stack memory\n");
            ctx.allocator.deallocate(unset_heap_bit(ptr), count * type_size);
        } break;
        case op2::alloc_ptr: {
            const auto type_size = read<std::uint64_t>(prog, ctx.prog_ptr);
            const auto ptr = ctx.allocator.allocate(type_size);
            push_value(ctx.stack, set_heap_bit(ptr));
        } break;
        case op2::dealloc_ptr: {
            const auto type_size = read<std::uint64_t>(prog, ctx.prog_ptr);
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            runtime_assert(is_heap_ptr(ptr), "cannot delete a pointer to stack memory\n");
            ctx.allocator.deallocate(unset_heap_bit(ptr), type_size);
        } break;
        case op2::jump: {
            const auto jump = read<std::uint64_t>(prog, ctx.prog_ptr);
            ctx.prog_ptr = jump;
        } break;
        case op2::jump_if_false: {
            const auto jump = read<std::uint64_t>(prog, ctx.prog_ptr);
            if (!pop_value<bool>(ctx.stack)) {
                ctx.prog_ptr = jump;
            }
        } break;
        case op2::ret: {
            const auto size = read<std::uint64_t>(prog, ctx.prog_ptr);
            const auto prev_base_ptr = read_value<std::uint64_t>(ctx.stack, ctx.base_ptr);
            const auto prev_prog_ptr = read_value<std::uint64_t>(ctx.stack, ctx.base_ptr + sizeof(std::uint64_t));
            
            std::memcpy(&ctx.stack[ctx.base_ptr], &ctx.stack[ctx.stack.size() - size], size);
            ctx.stack.resize(ctx.base_ptr + size);
            ctx.base_ptr = prev_base_ptr;
            ctx.prog_ptr = prev_prog_ptr;
        } break;
        case op2::function_call: {
            const auto ptr = read<std::uint64_t>(prog, ctx.prog_ptr);
            const auto args_size = read<std::uint64_t>(prog, ctx.prog_ptr);

            // Store the old base_ptr and prog_ptr so that they can be restored at the end of
            // the function.
            const auto new_base_ptr = ctx.stack.size() - args_size;
            write_value(ctx.stack, new_base_ptr, ctx.base_ptr);
            write_value(ctx.stack, new_base_ptr + sizeof(std::uint64_t), ctx.prog_ptr + 1); // Pos after function call
            
            ctx.base_ptr = new_base_ptr;
            ctx.prog_ptr = ptr; // Jump into the function
        } break;
        case op2::call: {
            const auto args_size = read<std::uint64_t>(prog, ctx.prog_ptr);

            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            // Store the old base_ptr and prog_ptr so that they can be restored at the end of
            // the function.
            const auto new_base_ptr = ctx.stack.size() - args_size;
            write_value(ctx.stack, new_base_ptr, ctx.base_ptr);
            write_value(ctx.stack, new_base_ptr + sizeof(std::uint64_t), ctx.prog_ptr + 1); // Pos after function call
            
            ctx.base_ptr = new_base_ptr;
            ctx.prog_ptr = ptr; // Jump into the function
        } break;
        case op2::builtin_call: {
            const auto id = read<std::uint64_t>(prog, ctx.prog_ptr);
            get_builtin(id).ptr(ctx);
        } break;
        case op2::assert: {
            const auto index = read<std::uint64_t>(prog, ctx.prog_ptr);
            const auto size = read<std::uint64_t>(prog, ctx.prog_ptr);
            if (!pop_value<bool>(ctx.stack)) {
                const auto m = std::string_view( // UB?
                    reinterpret_cast<const char*>(&ctx.rom[index]), size
                );
                runtime_error(m);
            }
        } break;

        case op2::char_eq: { binary_op<char, std::equal_to>(ctx); } break;
        case op2::char_ne: { binary_op<char, std::not_equal_to>(ctx); } break;

        case op2::i32_add: { binary_op<std::int32_t, std::plus>(ctx); } break;
        case op2::i32_sub: { binary_op<std::int32_t, std::minus>(ctx); } break;
        case op2::i32_mul: { binary_op<std::int32_t, std::multiplies>(ctx); } break;
        case op2::i32_div: { binary_op<std::int32_t, std::divides>(ctx); } break;
        case op2::i32_mod: { binary_op<std::int32_t, std::modulus>(ctx); } break;
        case op2::i32_eq:  { binary_op<std::int32_t, std::equal_to>(ctx); } break;
        case op2::i32_ne:  { binary_op<std::int32_t, std::not_equal_to>(ctx); } break;
        case op2::i32_lt:  { binary_op<std::int32_t, std::less>(ctx); } break;
        case op2::i32_le:  { binary_op<std::int32_t, std::less_equal>(ctx); } break;
        case op2::i32_gt:  { binary_op<std::int32_t, std::greater>(ctx); } break;
        case op2::i32_ge:  { binary_op<std::int32_t, std::greater_equal>(ctx); } break;

        case op2::i64_add: { binary_op<std::int64_t, std::plus>(ctx); } break;
        case op2::i64_sub: { binary_op<std::int64_t, std::minus>(ctx); } break;
        case op2::i64_mul: { binary_op<std::int64_t, std::multiplies>(ctx); } break;
        case op2::i64_div: { binary_op<std::int64_t, std::divides>(ctx); } break;
        case op2::i64_mod: { binary_op<std::int64_t, std::modulus>(ctx); } break;
        case op2::i64_eq:  { binary_op<std::int64_t, std::equal_to>(ctx); } break;
        case op2::i64_ne:  { binary_op<std::int64_t, std::not_equal_to>(ctx); } break;
        case op2::i64_lt:  { binary_op<std::int64_t, std::less>(ctx); } break;
        case op2::i64_le:  { binary_op<std::int64_t, std::less_equal>(ctx); } break;
        case op2::i64_gt:  { binary_op<std::int64_t, std::greater>(ctx); } break;
        case op2::i64_ge:  { binary_op<std::int64_t, std::greater_equal>(ctx); } break;

        case op2::u64_add: { binary_op<std::uint64_t, std::plus>(ctx); } break;
        case op2::u64_sub: { binary_op<std::uint64_t, std::minus>(ctx); } break;
        case op2::u64_mul: { binary_op<std::uint64_t, std::multiplies>(ctx); } break;
        case op2::u64_div: { binary_op<std::uint64_t, std::divides>(ctx); } break;
        case op2::u64_mod: { binary_op<std::uint64_t, std::modulus>(ctx); } break;
        case op2::u64_eq:  { binary_op<std::uint64_t, std::equal_to>(ctx); } break;
        case op2::u64_ne:  { binary_op<std::uint64_t, std::not_equal_to>(ctx); } break;
        case op2::u64_lt:  { binary_op<std::uint64_t, std::less>(ctx); } break;
        case op2::u64_le:  { binary_op<std::uint64_t, std::less_equal>(ctx); } break;
        case op2::u64_gt:  { binary_op<std::uint64_t, std::greater>(ctx); } break;
        case op2::u64_ge:  { binary_op<std::uint64_t, std::greater_equal>(ctx); } break;

        case op2::f64_add: { binary_op<double, std::plus>(ctx); } break;
        case op2::f64_sub: { binary_op<double, std::minus>(ctx); } break;
        case op2::f64_mul: { binary_op<double, std::multiplies>(ctx); } break;
        case op2::f64_div: { binary_op<double, std::divides>(ctx); } break;
        case op2::f64_eq:  { binary_op<double, std::equal_to>(ctx); } break;
        case op2::f64_ne:  { binary_op<double, std::not_equal_to>(ctx); } break;
        case op2::f64_lt:  { binary_op<double, std::less>(ctx); } break;
        case op2::f64_le:  { binary_op<double, std::less_equal>(ctx); } break;
        case op2::f64_gt:  { binary_op<double, std::greater>(ctx); } break;
        case op2::f64_ge:  { binary_op<double, std::greater_equal>(ctx); } break;

        case op2::bool_and: { binary_op<bool, std::logical_and>(ctx); } break;
        case op2::bool_or:  { binary_op<bool, std::logical_or>(ctx); } break;
        case op2::bool_eq:  { binary_op<bool, std::equal_to>(ctx); } break;
        case op2::bool_ne:  { binary_op<bool, std::not_equal_to>(ctx); } break;
        case op2::bool_not: { unary_op<bool, std::logical_not>(ctx); } break;

        case op2::i32_neg: { unary_op<std::int32_t, std::negate>(ctx); } break;
        case op2::i64_neg: { unary_op<std::int64_t, std::negate>(ctx); } break;
        case op2::f64_neg: { unary_op<double, std::negate>(ctx); } break;
        default: { runtime_error("unknown op code! ({})", static_cast<int>(op_code)); } break;
    }
}

auto print_op(const bytecode_program& prog, std::size_t ptr) -> std::size_t
{
    std::size_t start = ptr;
    print("[{:>3}] ", ptr);
    const auto op_code = static_cast<op2>(prog.code[ptr++]);
    switch (op_code) {
        // TODO: Pushing literals can just be memcpy's without casting, because we're
        // going from bytes to bytes
        case op2::push_literal_i32: {
            const auto value = read<std::int32_t>(prog, ptr);
            print("PUSH_LITERAL_I32: {}\n", value);
        } break;
        case op2::push_literal_i64: {
            const auto value = read<std::int64_t>(prog, ptr);
            print("PUSH_LITERAL_I64: {}\n", value);
        } break;
        case op2::push_literal_u64: {
            const auto value = read<std::uint64_t>(prog, ptr);
            print("PUSH_LITERAL_U64: {}\n", value);
        } break;
        case op2::push_literal_f64: {
            const auto value = read<double>(prog, ptr);
            print("PUSH_LITERAL_F64: {}\n", value);
        } break;
        case op2::push_literal_char: {
            const auto value = read<char>(prog, ptr);
            print("PUSH_LITERAL_CHAR: {}\n", value);
        } break;
        case op2::push_literal_bool: {
            const auto value = read<bool>(prog, ptr);
            print("PUSH_LITERAL_BOOL: {}\n", value);
        } break;
        case op2::push_literal_null: {
            print("PUSH_LITERAL_NULL\n");
        } break;
        case op2::push_global_addr: {
            const auto pos = read<std::uint64_t>(prog, ptr);
            print("PUSH_GLOBAL_ADDR: {}\n", pos);
        } break;
        case op2::push_local_addr: {
            const auto offset = read<std::uint64_t>(prog, ptr);
            print("PUSH_LOCAL_ADDR: base+{}\n", offset);
        } break;
        case op2::load: {
            const auto size = read<std::uint64_t>(prog, ptr);
            print("LOAD: {}\n", size);
        } break;
        case op2::save: {
            const auto size = read<std::uint64_t>(prog, ptr);
            print("SAVE: {}\n", size);
        } break;
        case op2::pop: {
            const auto size = read<std::uint64_t>(prog, ptr);
            print("POP: {}\n", size);
        } break;
        case op2::alloc_span: {
            const auto type_size = read<std::uint64_t>(prog, ptr);
            print("ALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op2::dealloc_span: {
            const auto type_size = read<std::uint64_t>(prog, ptr);
            print("DEALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op2::alloc_ptr: {
            const auto type_size = read<std::uint64_t>(prog, ptr);
            print("ALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op2::dealloc_ptr: {
            const auto type_size = read<std::uint64_t>(prog, ptr);
            print("DEALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op2::jump: {
            const auto jump = read<std::uint64_t>(prog, ptr);
            print("JUMP: jump={}\n", jump);
        } break;
        case op2::jump_if_false: {
            const auto jump = read<std::uint64_t>(prog, ptr);
            print("JUMP_IF_FALSE: jump={}\n", jump);
        } break;
        case op2::ret: {
            const auto type_size = read<std::uint64_t>(prog, ptr);
            print("RETURN: type_size={}\n", type_size);
        } break;
        case op2::function_call: {
            const auto func_ptr = read<std::uint64_t>(prog, ptr);
            const auto args_size = read<std::uint64_t>(prog, ptr);
            print("FUNCTION_CALL: func_ptr={} args_size={}\n", func_ptr, args_size);
        } break;
        case op2::call: {
            const auto args_size = read<std::uint64_t>(prog, ptr);
            print("RETURN: args_size={}\n", args_size);
        } break;
        case op2::builtin_call: {
            const auto id = read<std::uint64_t>(prog, ptr);
            const auto& b = get_builtin(id);
            print(
                "BUILTIN_CALL: {}({}) -> {}\n",
                b.name, format_comma_separated(b.args), b.return_type
            );
        } break;
        case op2::assert: {
            const auto index = read<std::uint64_t>(prog, ptr);
            const auto size = read<std::uint64_t>(prog, ptr);
            const auto m = std::string_view( // UB?
                reinterpret_cast<const char*>(&prog.rom[index]), size
            );
            print("ASSERT: msg={}\n", m);
        } break;
        case op2::char_eq: { print("CHAR_EQ\n"); } break;
        case op2::char_ne: { print("CHAR_NE\n"); } break;
        case op2::i32_add: { print("I32_ADD\n"); } break;
        case op2::i32_sub: { print("I32_SUB\n"); } break;
        case op2::i32_mul: { print("I32_MUL\n"); } break;
        case op2::i32_div: { print("I32_DIV\n"); } break;
        case op2::i32_mod: { print("I32_MOD\n"); } break;
        case op2::i32_eq:  { print("I32_EQ\n"); } break;
        case op2::i32_ne:  { print("I32_NE\n"); } break;
        case op2::i32_lt:  { print("I32_LT\n"); } break;
        case op2::i32_le:  { print("I32_LE\n"); } break;
        case op2::i32_gt:  { print("I32_GT\n"); } break;
        case op2::i32_ge:  { print("I32_GE\n"); } break;
        case op2::i64_add: { print("I64_ADD\n"); } break;
        case op2::i64_sub: { print("I64_SUB\n"); } break;
        case op2::i64_mul: { print("I64_MUL\n"); } break;
        case op2::i64_div: { print("I64_DIV\n"); } break;
        case op2::i64_mod: { print("I64_MOD\n"); } break;
        case op2::i64_eq:  { print("I64_EQ\n"); } break;
        case op2::i64_ne:  { print("I64_NE\n"); } break;
        case op2::i64_lt:  { print("I64_LT\n"); } break;
        case op2::i64_le:  { print("I64_LE\n"); } break;
        case op2::i64_gt:  { print("I64_GT\n"); } break;
        case op2::i64_ge:  { print("I64_GE\n"); } break;
        case op2::u64_add: { print("U64_ADD\n"); } break;
        case op2::u64_sub: { print("U64_SUB\n"); } break;
        case op2::u64_mul: { print("U64_MUL\n"); } break;
        case op2::u64_div: { print("U64_DIV\n"); } break;
        case op2::u64_mod: { print("U64_MOD\n"); } break;
        case op2::u64_eq:  { print("U64_EQ\n"); } break;
        case op2::u64_ne:  { print("U64_NE\n"); } break;
        case op2::u64_lt:  { print("U64_LT\n"); } break;
        case op2::u64_le:  { print("U64_LE\n"); } break;
        case op2::u64_gt:  { print("U64_GT\n"); } break;
        case op2::u64_ge:  { print("U64_GE\n"); } break;
        case op2::f64_add: { print("F64_ADD\n"); } break;
        case op2::f64_sub: { print("F64_SUB\n"); } break;
        case op2::f64_mul: { print("F64_MUL\n"); } break;
        case op2::f64_div: { print("F64_DIV\n"); } break;
        case op2::f64_eq:  { print("F64_EQ\n"); } break;
        case op2::f64_ne:  { print("F64_NE\n"); } break;
        case op2::f64_lt:  { print("F64_LT\n"); } break;
        case op2::f64_le:  { print("F64_LE\n"); } break;
        case op2::f64_gt:  { print("F64_GT\n"); } break;
        case op2::f64_ge:  { print("F64_GE\n"); } break;
        case op2::bool_and: { print("BOOL_AND\n"); } break;
        case op2::bool_or:  { print("BOOL_OR\n"); } break;
        case op2::bool_eq:  { print("BOOL_EQ\n"); } break;
        case op2::bool_ne:  { print("BOOL_NE\n"); } break;
        case op2::bool_not: { print("BOOL_NOT\n"); } break;
        case op2::i32_neg: { print("I32_NEG\n"); } break;
        case op2::i64_neg: { print("I64_NEG\n"); } break;
        case op2::f64_neg: { print("F64_NEG\n"); } break;
        default: {
            print("UNKNOWN\n");
            return 0;
        } break;
    }
    return ptr - start;
}

}

auto run_program(const bytecode_program& prog) -> void
{
    const auto timer = scope_timer{};

    bytecode_context ctx;
    ctx.rom = prog.rom;
    while (ctx.prog_ptr < prog.code.size()) {
        apply_op(prog, ctx);
    }

    if (ctx.allocator.bytes_allocated() > 0) {
        anzu::print("\n -> Heap Size: {}, fix your memory leak!\n", ctx.allocator.bytes_allocated());
    }
}

auto run_program_debug(const bytecode_program& prog) -> void
{
    const auto timer = scope_timer{};

    bytecode_context ctx;
    ctx.rom = prog.rom;
    while (ctx.prog_ptr < prog.code.size()) {
        print_op(prog, ctx.prog_ptr);
        apply_op(prog, ctx);
    }

    if (ctx.allocator.bytes_allocated() > 0) {
        anzu::print("\n -> Heap Size: {}, fix your memory leak!\n", ctx.allocator.bytes_allocated());
    }
}

auto print_program(const bytecode_program& prog) -> void
{
    auto ptr = std::size_t{0};
    while (ptr < prog.code.size()) {
        const auto offset = print_op(prog, ptr);
        if (offset == 0) return;
        ptr += offset;
    }
}

}