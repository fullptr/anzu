#include "runtime.hpp"
#include "functions.hpp"
#include "bytecode.hpp"

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
auto print_value(bytecode_context& ctx) -> void
{
    const auto obj = ctx.stack.pop<Type>();
    std::print("{}", obj);
}

template <typename T>
requires std::integral<T> || std::floating_point<T> || std::same_as<T, std::byte*> || std::same_as<T, op>
auto read_advance(bytecode_context& ctx) -> T
{
    auto& ptr = ctx.frames.back().prog_ptr;
    auto ret = T{};
    std::memcpy(&ret, &ctx.code[ptr], sizeof(T));
    ptr += sizeof(T);
    return ret;
}

auto apply_op(bytecode_context& ctx) -> void
{
    auto& frame = ctx.frames.back();
    const auto op_code = static_cast<op>(ctx.code[frame.prog_ptr++]);
    switch (op_code) {
        case op::push_char:
        case op::push_bool: {
            ctx.stack.push(read_advance<std::uint8_t>(ctx));
        } break;
        case op::push_i32: {
            ctx.stack.push(read_advance<std::uint32_t>(ctx));
        } break;
        case op::push_i64:
        case op::push_u64:
        case op::push_f64: {
            ctx.stack.push(read_advance<std::uint64_t>(ctx));
        } break;
        case op::push_string_literal: {
            const auto index = read_advance<std::uint64_t>(ctx);
            const auto size = read_advance<std::uint64_t>(ctx);
            ctx.stack.push(&ctx.rom[index]);
            ctx.stack.push(size);
        } break;
        case op::push_null: {
            ctx.stack.push(std::byte{0});
        } break;
        case op::push_ptr_global: {
            const auto offset = read_advance<std::uint64_t>(ctx);
            std::byte* ptr = &ctx.stack.at(offset);
            ctx.stack.push(ptr);
        } break;
        case op::push_ptr_local: {
            const auto offset = read_advance<std::uint64_t>(ctx);
            std::byte* ptr = &ctx.stack.at(frame.base_ptr + offset);
            ctx.stack.push(ptr);
        } break;
        case op::load: {
            const auto size = read_advance<std::uint64_t>(ctx);
            const auto ptr = ctx.stack.pop<std::byte*>();
            ctx.stack.push(ptr, size);
        } break;
        case op::save: {
            const auto size = read_advance<std::uint64_t>(ctx);
            const auto ptr = ctx.stack.pop<std::byte*>();
            ctx.stack.pop_and_save(ptr, size);
        } break;
        case op::pop: {
            const auto size = read_advance<std::uint64_t>(ctx);
            ctx.stack.resize(ctx.stack.size() - size);
        } break;
        case op::new_arena: {} break;
        case op::delete_arena: {} break;
        case op::alloc_span: {
            const auto type_size = read_advance<std::uint64_t>(ctx);
            const auto count = ctx.stack.pop<std::uint64_t>();
            const auto ptr = (std::byte*)std::malloc(count * type_size);
            ctx.heap_size += count * type_size;
            ctx.stack.push(ptr);
        } break;
        case op::dealloc_span: {
            const auto type_size = read_advance<std::uint64_t>(ctx);
            const auto count = ctx.stack.pop<std::uint64_t>();
            const auto ptr = ctx.stack.pop<std::byte*>();
            ctx.heap_size -= count * type_size;
            std::free(ptr);
        } break;
        case op::alloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(ctx);
            const auto ptr = (std::byte*)std::malloc(type_size);
            ctx.heap_size += type_size;
            ctx.stack.push(ptr);
        } break;
        case op::dealloc_ptr: {
            const auto type_size = read_advance<std::uint64_t>(ctx);
            const auto ptr = ctx.stack.pop<std::byte*>();
            ctx.heap_size -= type_size;
            std::free(ptr);
        } break;
        case op::jump: {
            frame.prog_ptr = read_advance<std::uint64_t>(ctx);
        } break;
        case op::jump_if_false: {
            const auto jump = read_advance<std::uint64_t>(ctx);
            if (!ctx.stack.pop<bool>()) frame.prog_ptr = jump;
        } break;
        case op::ret: {
            const auto size = read_advance<std::uint64_t>(ctx);
            std::memcpy(&ctx.stack.at(frame.base_ptr), &ctx.stack.at(ctx.stack.size() - size), size);
            ctx.stack.resize(frame.base_ptr + size);
            ctx.frames.pop_back();
        } break;
        case op::call: {
            const auto args_size = read_advance<std::uint64_t>(ctx);
            const auto prog_ptr = ctx.stack.pop<std::uint64_t>();
            ctx.frames.push_back(call_frame{
                .prog_ptr = prog_ptr,
                .base_ptr = ctx.stack.size() - args_size
            });
        } break;
        case op::builtin_call: {
            const auto id = read_advance<std::uint64_t>(ctx);
            get_builtin(id).ptr(ctx);
        } break;
        case op::assert: {
            const auto index = read_advance<std::uint64_t>(ctx);
            const auto size = read_advance<std::uint64_t>(ctx);
            if (!ctx.stack.pop<bool>()) {
                const auto data = &ctx.rom[index];
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
        case op::print_i32: { print_value<std::int32_t>(ctx); } break;
        case op::print_i64: { print_value<std::int64_t>(ctx); } break;
        case op::print_u64: { print_value<std::uint64_t>(ctx); } break;
        case op::print_f64: { print_value<double>(ctx); } break;
        case op::print_char_span: {
            const auto size = ctx.stack.pop<std::uint64_t>();
            const auto ptr = ctx.stack.pop<const char*>();
            std::print("{}", std::string_view{ptr, size});
        } break;

        default: { runtime_error("unknown op code! ({})", static_cast<int>(op_code)); } break;
    }
}

template <bool Debug>
auto run(const bytecode_program& prog) -> void
{
    const auto timer = scope_timer{};
    bytecode_context ctx{prog.code, prog.rom};
    ctx.frames.emplace_back();

    while (ctx.frames.back().prog_ptr < prog.code.size()) {
        if constexpr (Debug) {
            print_op(prog, ctx.frames.back().prog_ptr);
        }
        apply_op(ctx);
    }

    if (ctx.stack.size() > 0) {
        std::print("\n -> Stack Size: {}, bug in the compiler!\n", ctx.stack.size());
    }

    if (ctx.heap_size != 0) {
        std::print("\n -> Heap Size: {}, fix your memory leak!\n", ctx.heap_size);
    }
}

}

vm_stack::vm_stack(std::size_t size)
    : d_data{std::make_unique<std::byte[]>(size)}
    , d_max_size{size}
    , d_current_size{0}
{}

auto vm_stack::push(const std::byte* src, std::size_t count) -> void
{
    if (d_current_size + count > d_max_size) {
        std::print("Stack overflow\n");
        std::exit(27);
    }
    std::memcpy(&d_data[d_current_size], src, count);
    d_current_size += count;
}

auto vm_stack::pop_and_save(std::byte* dst, std::size_t count) -> void
{
    if (d_current_size < count) {
        std::print("Stack underflow\n");
        std::exit(28);
    }
    d_current_size -= count;
    std::memcpy(dst, &d_data[d_current_size], count);
}

auto vm_stack::size() const -> std::size_t { return d_current_size; }
auto vm_stack::at(std::size_t index) -> std::byte& { return d_data[index]; }
auto vm_stack::resize(std::size_t size) -> void { d_current_size = size; }

auto run_program(const bytecode_program& prog) -> void
{
    run<false>(prog);
}

auto run_program_debug(const bytecode_program& prog) -> void
{
    run<true>(prog);
}

}