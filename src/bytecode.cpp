#include "bytecode.hpp"
#include "object.hpp"
#include "functions.hpp"
#include "utility/memory.hpp"
#include "utility/common.hpp"

#include <concepts>
#include <utility>
#include <print>

namespace anzu {
namespace {

template <typename ...Args>
[[noreturn]] auto runtime_error(std::format_string<Args...> message, Args&&... args)
{
    const auto msg = std::format(message, std::forward<Args>(args)...);
    panic("runtime assertion failed! {}", msg);
}

template <typename Type, template <typename> typename Op>
auto unary_op(bytecode_context& ctx) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto obj = ctx.stack.pop<Type>();
    ctx.stack.push(op(obj));
}

template <typename Type, template <typename> typename Op>
auto binary_op(bytecode_context& ctx) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto rhs = ctx.stack.pop<Type>();
    const auto lhs = ctx.stack.pop<Type>();
    ctx.stack.push(op(lhs, rhs));
}

template <typename Type>
auto print_op(bytecode_context& ctx) -> void
{
    const auto obj = ctx.stack.pop<Type>();
    std::print("{}", obj);
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
    auto& frame = ctx.frames.back();
    for (std::size_t i = 0; i != N; ++i) {
        ctx.stack.push(prog.code[frame.prog_ptr++]);
    }
}

auto resolve_ptr(bytecode_context& ctx, std::size_t ptr) -> std::byte*
{
    if (is_heap_ptr(ptr)) return &ctx.heap[unset_heap_bit(ptr)];
    if (is_rom_ptr(ptr))  return &ctx.rom[unset_rom_bit(ptr)];
    return &ctx.stack.at(ptr);
}

auto apply_op(const bytecode_program& prog, bytecode_context& ctx) -> void
{
    auto& frame = ctx.frames.back();
    const auto op_code = static_cast<op>(prog.code[frame.prog_ptr++]);
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
            ctx.stack.push(std::byte{0});
        } break;
        case op::push_ptr_rel: {
            const auto offset = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            ctx.stack.push(frame.base_ptr + offset);
        } break;
        case op::load: {
            const auto size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            const auto raw_ptr = ctx.stack.pop<std::uint64_t>();
            const auto ptr = resolve_ptr(ctx, raw_ptr);

            for (std::size_t i = 0; i != size; ++i) {
                ctx.stack.push(*(ptr + i));
            }
        } break;
        case op::save: {
            const auto size = read_advance<std::uint64_t>(prog, frame.prog_ptr);

            const auto ptr = ctx.stack.pop<std::uint64_t>();
            if (is_heap_ptr(ptr)) {
                const auto heap_ptr = unset_heap_bit(ptr);
                std::memcpy(&ctx.heap[heap_ptr], &ctx.stack.at(ctx.stack.size() - size), size);
                ctx.stack.pop_n(size);
            }
            else if (is_rom_ptr(ptr)) {
                runtime_error("cannot assign into read only memory");
            }
            else {
                panic_if(ptr + size > ctx.stack.size(), "tried to access invalid memory address {}", ptr);
                if (ptr + size < ctx.stack.size()) {
                    std::memcpy(&ctx.stack.at(ptr), &ctx.stack.at(ctx.stack.size() - size), size);
                    ctx.stack.pop_n(size);
                }
            }
        } break;
        case op::pop: {
            const auto size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            ctx.stack.pop_n(size);
        } break;
        case op::alloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            const auto count = ctx.stack.pop<std::uint64_t>();
            const auto ptr = ctx.allocator.allocate(count * type_size);
            ctx.stack.push(set_heap_bit(ptr));
        } break;
        case op::dealloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            const auto count = ctx.stack.pop<std::uint64_t>();
            const auto ptr = ctx.stack.pop<std::uint64_t>();
            panic_if(!is_heap_ptr(ptr), "cannot delete a span to non-heap memory");
            ctx.allocator.deallocate(unset_heap_bit(ptr), count * type_size);
        } break;
        case op::alloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            const auto ptr = ctx.allocator.allocate(type_size);
            ctx.stack.push(set_heap_bit(ptr));
        } break;
        case op::dealloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            const auto ptr = ctx.stack.pop<std::uint64_t>();
            panic_if(!is_heap_ptr(ptr), "cannot delete a pointer to non-heap memory");
            ctx.allocator.deallocate(unset_heap_bit(ptr), type_size);
        } break;
        case op::jump: {
            frame.prog_ptr = read_advance<std::uint64_t>(prog, frame.prog_ptr);
        } break;
        case op::jump_if_false: {
            const auto jump = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            if (!ctx.stack.pop<bool>()) {
                frame.prog_ptr = jump;
            }
        } break;
        case op::ret: {
            const auto size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            std::memcpy(&ctx.stack.at(frame.base_ptr), &ctx.stack.at(ctx.stack.size() - size), size);
            ctx.stack.resize(frame.base_ptr + size);

            ctx.frames.pop_back();
        } break;
        case op::call: {
            const auto args_size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            const auto prog_ptr = ctx.stack.pop<std::uint64_t>();
            ctx.frames.push_back(call_frame{
                .prog_ptr = prog_ptr,
                .base_ptr = ctx.stack.size() - args_size
            });

        } break;
        case op::builtin_call: {
            const auto id = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            get_builtin(id).ptr(ctx);
        } break;
        case op::assert: {
            const auto index = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            const auto size = read_advance<std::uint64_t>(prog, frame.prog_ptr);
            if (!ctx.stack.pop<bool>()) {
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

        case op::print_null: {
            ctx.stack.pop<std::byte>(); // pops the null byte
            std::print("null");
        } break;
        case op::print_bool: {
            const auto b = ctx.stack.pop<bool>();
            std::print("{}", b ? "true" : "false");
        } break;
        case op::print_char: {
            const auto c = ctx.stack.pop<char>();
            std::print("{}", c);
        } break;
        case op::print_i32: { print_op<std::int32_t>(ctx); } break;
        case op::print_i64: { print_op<std::int64_t>(ctx); } break;
        case op::print_u64: { print_op<std::uint64_t>(ctx); } break;
        case op::print_f64: { print_op<double>(ctx); } break;
        case op::print_char_span: {
            const auto size = ctx.stack.pop<std::uint64_t>();
            const auto raw_ptr = ctx.stack.pop<std::uint64_t>();
            const auto ptr = reinterpret_cast<const char*>(resolve_ptr(ctx, raw_ptr));
            std::print("{}", std::string_view{ptr, size});
        } break;

        default: { runtime_error("unknown op code! ({})", static_cast<int>(op_code)); } break;
    }
}

auto print_op(const bytecode_program& prog, std::size_t ptr) -> std::size_t
{
    std::size_t start = ptr;
    std::print("[{:>3}] ", ptr);
    const auto op_code = static_cast<op>(prog.code[ptr++]);
    switch (op_code) {
        case op::push_i32: {
            const auto value = read_advance<std::int32_t>(prog, ptr);
            std::print("PUSH_I32: {}\n", value);
        } break;
        case op::push_i64: {
            const auto value = read_advance<std::int64_t>(prog, ptr);
            std::print("PUSH_I64: {}\n", value);
        } break;
        case op::push_u64: {
            const auto value = read_advance<std::uint64_t>(prog, ptr);
            std::print("PUSH_U64: {}\n", value);
        } break;
        case op::push_f64: {
            const auto value = read_advance<double>(prog, ptr);
            std::print("PUSH_F64: {}\n", value);
        } break;
        case op::push_char: {
            const auto value = read_advance<char>(prog, ptr);
            std::print("PUSH_CHAR: {}\n", value);
        } break;
        case op::push_bool: {
            const auto value = read_advance<bool>(prog, ptr);
            std::print("PUSH_BOOL: {}\n", value);
        } break;
        case op::push_null: {
            std::print("PUSH_NULL\n");
        } break;
        case op::push_string_literal: {
            const auto index = unset_rom_bit(read_advance<std::uint64_t>(prog, ptr));
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            const auto data = reinterpret_cast<const char*>(&prog.rom[index]);
            const auto m = std::string_view(data, size);
            std::print("PUSH_STRING_LITERAL: '{}'\n", m);
        } break;
        case op::push_ptr: {
            const auto pos = read_advance<std::uint64_t>(prog, ptr);
            if (is_heap_ptr(pos)) {
                std::print("PUSH_PTR: {} (HEAP)\n", unset_heap_bit(pos));
            }
            else if (is_rom_ptr(pos)) {
                std::print("PUSH_PTR: {} (ROM)\n", unset_rom_bit(pos));
            }
            else {
                std::print("PUSH_PTR: {} (STACK)\n", pos);
            }
        } break;
        case op::push_ptr_rel: {
            const auto offset = read_advance<std::uint64_t>(prog, ptr);
            std::print("PUSH_PTR_REL: base_ptr + {}\n", offset);
        } break;
        case op::load: {
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            std::print("LOAD: {}\n", size);
        } break;
        case op::save: {
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            std::print("SAVE: {}\n", size);
        } break;
        case op::pop: {
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            std::print("POP: {}\n", size);
        } break;
        case op::alloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            std::print("ALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op::dealloc_span: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            std::print("DEALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op::alloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            std::print("ALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op::dealloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            std::print("DEALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op::jump: {
            const auto jump = read_advance<std::uint64_t>(prog, ptr);
            std::print("JUMP: jump={}\n", jump);
        } break;
        case op::jump_if_false: {
            const auto jump = read_advance<std::uint64_t>(prog, ptr);
            std::print("JUMP_IF_FALSE: jump={}\n", jump);
        } break;
        case op::ret: {
            const auto type_size = read_advance<std::uint64_t>(prog, ptr);
            std::print("RETURN: type_size={}\n", type_size);
        } break;
        case op::call: {
            const auto args_size = read_advance<std::uint64_t>(prog, ptr);
            std::print("CALL: args_size={}\n", args_size);
        } break;
        case op::builtin_call: {
            const auto id = read_advance<std::uint64_t>(prog, ptr);
            const auto& b = get_builtin(id);
            std::print("BUILTIN_CALL: {}({}) -> {}\n",
                  b.name, format_comma_separated(b.args), b.return_type);
        } break;
        case op::assert: {
            const auto index = read_advance<std::uint64_t>(prog, ptr);
            const auto size = read_advance<std::uint64_t>(prog, ptr);
            const auto data = reinterpret_cast<const char*>(&prog.rom[index]);
            std::print("ASSERT: msg={}\n", std::string_view{data, size});
        } break;
        case op::char_eq: { std::print("CHAR_EQ\n"); } break;
        case op::char_ne: { std::print("CHAR_NE\n"); } break;
        case op::i32_add: { std::print("I32_ADD\n"); } break;
        case op::i32_sub: { std::print("I32_SUB\n"); } break;
        case op::i32_mul: { std::print("I32_MUL\n"); } break;
        case op::i32_div: { std::print("I32_DIV\n"); } break;
        case op::i32_mod: { std::print("I32_MOD\n"); } break;
        case op::i32_eq:  { std::print("I32_EQ\n"); } break;
        case op::i32_ne:  { std::print("I32_NE\n"); } break;
        case op::i32_lt:  { std::print("I32_LT\n"); } break;
        case op::i32_le:  { std::print("I32_LE\n"); } break;
        case op::i32_gt:  { std::print("I32_GT\n"); } break;
        case op::i32_ge:  { std::print("I32_GE\n"); } break;
        case op::i64_add: { std::print("I64_ADD\n"); } break;
        case op::i64_sub: { std::print("I64_SUB\n"); } break;
        case op::i64_mul: { std::print("I64_MUL\n"); } break;
        case op::i64_div: { std::print("I64_DIV\n"); } break;
        case op::i64_mod: { std::print("I64_MOD\n"); } break;
        case op::i64_eq:  { std::print("I64_EQ\n"); } break;
        case op::i64_ne:  { std::print("I64_NE\n"); } break;
        case op::i64_lt:  { std::print("I64_LT\n"); } break;
        case op::i64_le:  { std::print("I64_LE\n"); } break;
        case op::i64_gt:  { std::print("I64_GT\n"); } break;
        case op::i64_ge:  { std::print("I64_GE\n"); } break;
        case op::u64_add: { std::print("U64_ADD\n"); } break;
        case op::u64_sub: { std::print("U64_SUB\n"); } break;
        case op::u64_mul: { std::print("U64_MUL\n"); } break;
        case op::u64_div: { std::print("U64_DIV\n"); } break;
        case op::u64_mod: { std::print("U64_MOD\n"); } break;
        case op::u64_eq:  { std::print("U64_EQ\n"); } break;
        case op::u64_ne:  { std::print("U64_NE\n"); } break;
        case op::u64_lt:  { std::print("U64_LT\n"); } break;
        case op::u64_le:  { std::print("U64_LE\n"); } break;
        case op::u64_gt:  { std::print("U64_GT\n"); } break;
        case op::u64_ge:  { std::print("U64_GE\n"); } break;
        case op::f64_add: { std::print("F64_ADD\n"); } break;
        case op::f64_sub: { std::print("F64_SUB\n"); } break;
        case op::f64_mul: { std::print("F64_MUL\n"); } break;
        case op::f64_div: { std::print("F64_DIV\n"); } break;
        case op::f64_eq:  { std::print("F64_EQ\n"); } break;
        case op::f64_ne:  { std::print("F64_NE\n"); } break;
        case op::f64_lt:  { std::print("F64_LT\n"); } break;
        case op::f64_le:  { std::print("F64_LE\n"); } break;
        case op::f64_gt:  { std::print("F64_GT\n"); } break;
        case op::f64_ge:  { std::print("F64_GE\n"); } break;
        case op::bool_and: { std::print("BOOL_AND\n"); } break;
        case op::bool_or:  { std::print("BOOL_OR\n"); } break;
        case op::bool_eq:  { std::print("BOOL_EQ\n"); } break;
        case op::bool_ne:  { std::print("BOOL_NE\n"); } break;
        case op::bool_not: { std::print("BOOL_NOT\n"); } break;
        case op::i32_neg: { std::print("I32_NEG\n"); } break;
        case op::i64_neg: { std::print("I64_NEG\n"); } break;
        case op::f64_neg: { std::print("F64_NEG\n"); } break;
        case op::print_null: { std::print("PRINT_NULL\n"); } break;
        case op::print_bool: { std::print("PRINT_BOOL\n"); } break;
        case op::print_char: { std::print("PRINT_CHAR\n"); } break;
        case op::print_i32: { std::print("PRINT_I32\n"); } break;
        case op::print_i64: { std::print("PRINT_I64\n"); } break;
        case op::print_u64: { std::print("PRINT_U64\n"); } break;
        case op::print_f64: { std::print("PRINT_F64\n"); } break;
        case op::print_char_span: { std::print("PRINT_STRING_LITERAL\n");
        } break;
        default: {
            std::print("UNKNOWN\n");
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
    for (char c : prog.rom) {
        ctx.rom.push_back(static_cast<std::byte>(c));
    }
    ctx.frames.emplace_back();
    while (ctx.frames.back().prog_ptr < prog.code.size()) {
        apply_op(prog, ctx);
    }

    if (ctx.stack.size() > 0) {
        std::print("\n -> Stack Size: {}, bug in the compiler!\n", ctx.stack.size());
    }

    if (ctx.allocator.bytes_allocated() > 0) {
        std::print("\n -> Heap Size: {}, fix your memory leak!\n", ctx.allocator.bytes_allocated());
    }
}

auto run_program_debug(const bytecode_program& prog) -> void
{
    const auto timer = scope_timer{};

    bytecode_context ctx;
    for (char c : prog.rom) {
        ctx.rom.push_back(static_cast<std::byte>(c));
    }
    ctx.frames.emplace_back();
    while (ctx.frames.back().prog_ptr < prog.code.size()) {
        print_op(prog, ctx.frames.back().prog_ptr);
        apply_op(prog, ctx);
    }

    if (ctx.allocator.bytes_allocated() > 0) {
        std::print("\n -> Heap Size: {}, fix your memory leak!\n", ctx.allocator.bytes_allocated());
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