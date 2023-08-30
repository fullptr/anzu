#include "bytecode.hpp"
#include "object.hpp"
#include "functions.hpp"
#include "utility/scope_timer.hpp"
#include "utility/print.hpp"
#include "utility/memory.hpp"

#include <concepts>
#include <utility>

namespace anzu {
namespace {

template <typename ...Args>
auto runtime_assert(bool condition, std::format_string<Args...> msg, Args&&... args)
{
    if (!condition) {
        anzu::print(msg, std::forward<Args>(args)...);
        std::exit(1);
    }
}


template <typename ...Args>
[[noreturn]] auto runtime_error(std::format_string<Args...> message, Args&&... args)
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
    print("obj = {}\n", obj);
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
auto read_advance(const bytecode_program& prog, std::size_t& ptr) -> T
{
    auto ret = T{0};
    std::memcpy(&ret, &prog.code[ptr], sizeof(T));
    ptr += sizeof(T);
    return ret;
}

template <std::size_t N>
auto push_bytes_from_program(bytecode_context& ctx, const bytecode_program& prog) -> void
{
    for (std::size_t i = 0; i != N; ++i) {
        ctx.stack.push_back(prog.code[ctx.prog_ptr++]);
    }
}

auto apply_op(const bytecode_program& prog, bytecode_context& ctx) -> void
{
    const auto op_code = static_cast<op>(prog.code[ctx.prog_ptr++]);
    switch (op_code) {
        case op::push_char:
        case op::push_bool: {
            push_bytes_from_program<1>(ctx, prog);
        } break;
        case op::push_i32: {
            push_bytes_from_program<4>(ctx, prog);
        } break;
        case op::push_i64:
        case op::push_u64:
        case op::push_f64:
        case op::push_ptr: {
            push_bytes_from_program<8>(ctx, prog);
        } break;
        case op::push_string_literal: {
            push_bytes_from_program<16>(ctx, prog);
        } break;
        case op::push_null: {
            push_value(ctx.stack, std::byte{0});
        } break;
        case op::push_ptr_rel: {
            const auto offset = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            push_value(ctx.stack, ctx.base_ptr + offset);
        } break;
        case op::push_call_frame: {
            push_value(ctx.stack, std::uint64_t{0});
            push_value(ctx.stack, std::uint64_t{0});
        } break;
        case op::load: {
            const auto size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);

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
        case op::save: {
            const auto size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);

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
        case op::pop: {
            const auto size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            ctx.stack.resize(ctx.stack.size() - size);
        } break;
        case op::alloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            const auto count = pop_value<std::uint64_t>(ctx.stack);
            const auto ptr = ctx.allocator.allocate(count * type_size);
            push_value(ctx.stack, set_heap_bit(ptr));
        } break;
        case op::dealloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            const auto count = pop_value<std::uint64_t>(ctx.stack);
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            runtime_assert(is_heap_ptr(ptr), "cannot delete a span to stack memory\n");
            ctx.allocator.deallocate(unset_heap_bit(ptr), count * type_size);
        } break;
        case op::alloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            const auto ptr = ctx.allocator.allocate(type_size);
            push_value(ctx.stack, set_heap_bit(ptr));
        } break;
        case op::dealloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            runtime_assert(is_heap_ptr(ptr), "cannot delete a pointer to stack memory\n");
            ctx.allocator.deallocate(unset_heap_bit(ptr), type_size);
        } break;
        case op::jump: {
            ctx.prog_ptr = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
        } break;
        case op::jump_if_false: {
            const auto jump = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            if (!pop_value<bool>(ctx.stack)) {
                ctx.prog_ptr = jump;
            }
        } break;
        case op::ret: {
            const auto size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            const auto prev_base_ptr = read_value<std::uint64_t>(ctx.stack, ctx.base_ptr);
            const auto prev_prog_ptr = read_value<std::uint64_t>(ctx.stack, ctx.base_ptr + sizeof(std::uint64_t));
            
            std::memcpy(&ctx.stack[ctx.base_ptr], &ctx.stack[ctx.stack.size() - size], size);
            ctx.stack.resize(ctx.base_ptr + size);
            ctx.base_ptr = prev_base_ptr;
            ctx.prog_ptr = prev_prog_ptr;
        } break;
        case op::call: {
            const auto args_size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);

            // Store the old base_ptr and prog_ptr so that they can be restored at the end of
            // the function.
            const auto new_base_ptr = ctx.stack.size() - args_size;
            write_value(ctx.stack, new_base_ptr, ctx.base_ptr);
            write_value(ctx.stack, new_base_ptr + sizeof(std::uint64_t), ctx.prog_ptr);
            
            ctx.base_ptr = new_base_ptr;
            ctx.prog_ptr = ptr; // Jump into the function
        } break;
        case op::builtin_call: {
            const auto id = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            get_builtin(id).ptr(ctx);
        } break;
        case op::assert: {
            const auto index = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            const auto size = read_advance<std::uint64_t>(prog, ctx.prog_ptr);
            if (!pop_value<bool>(ctx.stack)) {
                const auto data = reinterpret_cast<const char*>(&prog.rom[index]);
                runtime_error("{}", std::string_view{data, size});
            }
        } break;

        case op::char_eq: { binary_op<char, std::equal_to>(ctx); } break;
        case op::char_ne: { binary_op<char, std::not_equal_to>(ctx); } break;

        case op::i32_add: { binary_op<std::int32_t, std::plus>(ctx); } break;
        case op::i32_sub: { binary_op<std::int32_t, std::minus>(ctx); } break;
        case op::i32_mul: { binary_op<std::int32_t, std::multiplies>(ctx); } break;
        case op::i32_div: { binary_op<std::int32_t, std::divides>(ctx); } break;
        case op::i32_mod: { binary_op<std::int32_t, std::modulus>(ctx); } break;
        case op::i32_eq:  { binary_op<std::int32_t, std::equal_to>(ctx); } break;
        case op::i32_ne:  { binary_op<std::int32_t, std::not_equal_to>(ctx); } break;
        case op::i32_lt:  { binary_op<std::int32_t, std::less>(ctx); } break;
        case op::i32_le:  { binary_op<std::int32_t, std::less_equal>(ctx); } break;
        case op::i32_gt:  { binary_op<std::int32_t, std::greater>(ctx); } break;
        case op::i32_ge:  { binary_op<std::int32_t, std::greater_equal>(ctx); } break;

        case op::i64_add: { binary_op<std::int64_t, std::plus>(ctx); } break;
        case op::i64_sub: { binary_op<std::int64_t, std::minus>(ctx); } break;
        case op::i64_mul: { binary_op<std::int64_t, std::multiplies>(ctx); } break;
        case op::i64_div: { binary_op<std::int64_t, std::divides>(ctx); } break;
        case op::i64_mod: { binary_op<std::int64_t, std::modulus>(ctx); } break;
        case op::i64_eq:  { binary_op<std::int64_t, std::equal_to>(ctx); } break;
        case op::i64_ne:  { binary_op<std::int64_t, std::not_equal_to>(ctx); } break;
        case op::i64_lt:  { binary_op<std::int64_t, std::less>(ctx); } break;
        case op::i64_le:  { binary_op<std::int64_t, std::less_equal>(ctx); } break;
        case op::i64_gt:  { binary_op<std::int64_t, std::greater>(ctx); } break;
        case op::i64_ge:  { binary_op<std::int64_t, std::greater_equal>(ctx); } break;

        case op::u64_add: { binary_op<std::uint64_t, std::plus>(ctx); } break;
        case op::u64_sub: { binary_op<std::uint64_t, std::minus>(ctx); } break;
        case op::u64_mul: { binary_op<std::uint64_t, std::multiplies>(ctx); } break;
        case op::u64_div: { binary_op<std::uint64_t, std::divides>(ctx); } break;
        case op::u64_mod: { binary_op<std::uint64_t, std::modulus>(ctx); } break;
        case op::u64_eq:  { binary_op<std::uint64_t, std::equal_to>(ctx); } break;
        case op::u64_ne:  { binary_op<std::uint64_t, std::not_equal_to>(ctx); } break;
        case op::u64_lt:  { binary_op<std::uint64_t, std::less>(ctx); } break;
        case op::u64_le:  { binary_op<std::uint64_t, std::less_equal>(ctx); } break;
        case op::u64_gt:  { binary_op<std::uint64_t, std::greater>(ctx); } break;
        case op::u64_ge:  { binary_op<std::uint64_t, std::greater_equal>(ctx); } break;

        case op::f64_add: { binary_op<double, std::plus>(ctx); } break;
        case op::f64_sub: { binary_op<double, std::minus>(ctx); } break;
        case op::f64_mul: { binary_op<double, std::multiplies>(ctx); } break;
        case op::f64_div: { binary_op<double, std::divides>(ctx); } break;
        case op::f64_eq:  { binary_op<double, std::equal_to>(ctx); } break;
        case op::f64_ne:  { binary_op<double, std::not_equal_to>(ctx); } break;
        case op::f64_lt:  { binary_op<double, std::less>(ctx); } break;
        case op::f64_le:  { binary_op<double, std::less_equal>(ctx); } break;
        case op::f64_gt:  { binary_op<double, std::greater>(ctx); } break;
        case op::f64_ge:  { binary_op<double, std::greater_equal>(ctx); } break;

        case op::bool_and: { binary_op<bool, std::logical_and>(ctx); } break;
        case op::bool_or:  { binary_op<bool, std::logical_or>(ctx); } break;
        case op::bool_eq:  { binary_op<bool, std::equal_to>(ctx); } break;
        case op::bool_ne:  { binary_op<bool, std::not_equal_to>(ctx); } break;
        case op::bool_not: { unary_op<bool, std::logical_not>(ctx); } break;

        case op::i32_neg: { unary_op<std::int32_t, std::negate>(ctx); } break;
        case op::i64_neg: { unary_op<std::int64_t, std::negate>(ctx); } break;
        case op::f64_neg: { unary_op<double, std::negate>(ctx); } break;
        default: { runtime_error("unknown op code! ({})", static_cast<int>(op_code)); } break;
    }
}

auto print_op(const bytecode_program& prog, std::size_t ptr) -> std::size_t
{
    std::size_t start = ptr;
    print("[{:>3}] ", ptr);
    const auto op_code = static_cast<op>(prog.code[ptr++]);
    switch (op_code) {
        case op::push_i32: {
            const auto value = read_advance<std::int32_t>(prog, ptr);
            print("PUSH_I32: {}\n", value);
        } break;
        case op::push_i64: {
            const auto value = read_advance<std::int64_t>(prog, ptr);
            print("PUSH_I64: {}\n", value);
        } break;
        case op::push_u64: {
            const auto value = read_advance<std::uint64_t>(prog, ptr);
            print("PUSH_U64: {}\n", value);
        } break;
        case op::push_f64: {
            const auto value = read_advance<double>(prog, ptr);
            print("PUSH_F64: {}\n", value);
        } break;
        case op::push_char: {
            const auto value = read_advance<char>(prog, ptr);
            print("PUSH_CHAR: {}\n", value);
        } break;
        case op::push_bool: {
            const auto value = read_advance<bool>(prog, ptr);
            print("PUSH_BOOL: {}\n", value);
        } break;
        case op::push_null: {
            print("PUSH_NULL\n");
        } break;
        case op::push_string_literal: {
            const auto index = unset_rom_bit(read_advance<std::uint64_t>(prog, ptr));
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            const auto data = reinterpret_cast<const char*>(&prog.rom[index]);
            const auto m = std::string_view(data, size);
            print("PUSH_STRING_LITERAL: '{}'\n", m);
        } break;
        case op::push_ptr: {
            const auto pos = read_advance<std::uint64_t>(prog, ptr);
            if (is_heap_ptr(pos)) {
                print("PUSH_PTR: {} (HEAP)\n", unset_heap_bit(pos));
            }
            else if (is_rom_ptr(pos)) {
                print("PUSH_PTR: {} (ROM)\n", unset_rom_bit(pos));
            }
            else {
                print("PUSH_PTR: {} (STACK)\n", pos);
            }
        } break;
        case op::push_ptr_rel: {
            const auto offset = read_advance<std::uint64_t>(prog, ptr);
            print("PUSH_PTR_REL: base_ptr + {}\n", offset);
        } break;
        case op::push_call_frame: {
            print("PUSH_CALL_FRAME\n");
        } break;
        case op::load: {
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            print("LOAD: {}\n", size);
        } break;
        case op::save: {
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            print("SAVE: {}\n", size);
        } break;
        case op::pop: {
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            print("POP: {}\n", size);
        } break;
        case op::alloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            print("ALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op::dealloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            print("DEALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op::alloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            print("ALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op::dealloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            print("DEALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op::jump: {
            const auto jump = read_advance<std::uint64_t>(prog, ptr);
            print("JUMP: jump={}\n", jump);
        } break;
        case op::jump_if_false: {
            const auto jump = read_advance<std::uint64_t>(prog, ptr);
            print("JUMP_IF_FALSE: jump={}\n", jump);
        } break;
        case op::ret: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            print("RETURN: type_size={}\n", type_size);
        } break;
        case op::call: {
            const auto args_size = read_advance<std::uint64_t>(prog, ptr);
            print("CALL: args_size={}\n", args_size);
        } break;
        case op::builtin_call: {
            const auto id = read_advance<std::uint64_t>(prog, ptr);
            const auto& b = get_builtin(id);
            print("BUILTIN_CALL: {}({}) -> {}\n",
                  b.name, format_comma_separated(b.args), b.return_type);
        } break;
        case op::assert: {
            const auto index = read_advance<std::uint64_t>(prog, ptr);
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            const auto data = reinterpret_cast<const char*>(&prog.rom[index]);
            print("ASSERT: msg={}\n", std::string_view{data, size});
        } break;
        case op::char_eq: { print("CHAR_EQ\n"); } break;
        case op::char_ne: { print("CHAR_NE\n"); } break;
        case op::i32_add: { print("I32_ADD\n"); } break;
        case op::i32_sub: { print("I32_SUB\n"); } break;
        case op::i32_mul: { print("I32_MUL\n"); } break;
        case op::i32_div: { print("I32_DIV\n"); } break;
        case op::i32_mod: { print("I32_MOD\n"); } break;
        case op::i32_eq:  { print("I32_EQ\n"); } break;
        case op::i32_ne:  { print("I32_NE\n"); } break;
        case op::i32_lt:  { print("I32_LT\n"); } break;
        case op::i32_le:  { print("I32_LE\n"); } break;
        case op::i32_gt:  { print("I32_GT\n"); } break;
        case op::i32_ge:  { print("I32_GE\n"); } break;
        case op::i64_add: { print("I64_ADD\n"); } break;
        case op::i64_sub: { print("I64_SUB\n"); } break;
        case op::i64_mul: { print("I64_MUL\n"); } break;
        case op::i64_div: { print("I64_DIV\n"); } break;
        case op::i64_mod: { print("I64_MOD\n"); } break;
        case op::i64_eq:  { print("I64_EQ\n"); } break;
        case op::i64_ne:  { print("I64_NE\n"); } break;
        case op::i64_lt:  { print("I64_LT\n"); } break;
        case op::i64_le:  { print("I64_LE\n"); } break;
        case op::i64_gt:  { print("I64_GT\n"); } break;
        case op::i64_ge:  { print("I64_GE\n"); } break;
        case op::u64_add: { print("U64_ADD\n"); } break;
        case op::u64_sub: { print("U64_SUB\n"); } break;
        case op::u64_mul: { print("U64_MUL\n"); } break;
        case op::u64_div: { print("U64_DIV\n"); } break;
        case op::u64_mod: { print("U64_MOD\n"); } break;
        case op::u64_eq:  { print("U64_EQ\n"); } break;
        case op::u64_ne:  { print("U64_NE\n"); } break;
        case op::u64_lt:  { print("U64_LT\n"); } break;
        case op::u64_le:  { print("U64_LE\n"); } break;
        case op::u64_gt:  { print("U64_GT\n"); } break;
        case op::u64_ge:  { print("U64_GE\n"); } break;
        case op::f64_add: { print("F64_ADD\n"); } break;
        case op::f64_sub: { print("F64_SUB\n"); } break;
        case op::f64_mul: { print("F64_MUL\n"); } break;
        case op::f64_div: { print("F64_DIV\n"); } break;
        case op::f64_eq:  { print("F64_EQ\n"); } break;
        case op::f64_ne:  { print("F64_NE\n"); } break;
        case op::f64_lt:  { print("F64_LT\n"); } break;
        case op::f64_le:  { print("F64_LE\n"); } break;
        case op::f64_gt:  { print("F64_GT\n"); } break;
        case op::f64_ge:  { print("F64_GE\n"); } break;
        case op::bool_and: { print("BOOL_AND\n"); } break;
        case op::bool_or:  { print("BOOL_OR\n"); } break;
        case op::bool_eq:  { print("BOOL_EQ\n"); } break;
        case op::bool_ne:  { print("BOOL_NE\n"); } break;
        case op::bool_not: { print("BOOL_NOT\n"); } break;
        case op::i32_neg: { print("I32_NEG\n"); } break;
        case op::i64_neg: { print("I64_NEG\n"); } break;
        case op::f64_neg: { print("F64_NEG\n"); } break;
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

    if (ctx.stack.size() > 0) {
        anzu::print("\n -> Stack Size: {}, bug in the compiler!\n", ctx.stack.size());
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