#include "runtime.hpp"
#include "object.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/scope_timer.hpp"
#include "utility/memory.hpp"

#include <chrono>
#include <algorithm>
#include <functional>
#include <utility>

namespace anzu {

template <typename ...Args>
auto runtime_assert(bool condition, std::string_view msg, Args&&... args)
{
    if (!condition) {
        anzu::print(msg, std::forward<Args>(args)...);
        std::exit(1);
    }
}

auto runtime_error(std::string_view message)
{
    anzu::print("Runtime assertion failed! {}\n", message);
    std::exit(1);
}

template <typename Type, template <typename> typename Op>
auto unary_op(runtime_context& ctx) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto obj = pop_value<Type>(ctx.stack);
    push_value(ctx.stack, op(obj));
    ++ctx.prog_ptr;
}

template <typename Type, template <typename> typename Op>
auto binary_op(runtime_context& ctx) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto rhs = pop_value<Type>(ctx.stack);
    const auto lhs = pop_value<Type>(ctx.stack);
    push_value(ctx.stack, op(lhs, rhs));
    ++ctx.prog_ptr;
}

auto apply_op(runtime_context& ctx, const op& op_code) -> void
{
    std::visit(overloaded {
        [&](op_push_literal_i32 op) {
            push_value(ctx.stack, op.value);
            ++ctx.prog_ptr;
        },
        [&](op_push_literal_i64 op) {
            push_value(ctx.stack, op.value);
            ++ctx.prog_ptr;
        },
        [&](op_push_literal_u64 op) {
            push_value(ctx.stack, op.value);
            ++ctx.prog_ptr;
        },
        [&](op_push_literal_f64 op) {
            push_value(ctx.stack, op.value);
            ++ctx.prog_ptr;
        },
        [&](op_push_literal_char op) {
            push_value(ctx.stack, op.value);
            ++ctx.prog_ptr;
        },
        [&](op_push_literal_bool op) {
            push_value(ctx.stack, op.value);
            ++ctx.prog_ptr;
        },
        [&](op_push_literal_null) {
            ctx.stack.push_back(std::byte{0});
            ++ctx.prog_ptr;
        },
        [&](op_push_global_addr op) {
            push_value(ctx.stack, op.position);
            ++ctx.prog_ptr;
        },
        [&](op_push_local_addr op) {
            push_value(ctx.stack, ctx.base_ptr + op.offset);
            ++ctx.prog_ptr;
        },
        [&](op_load op) {
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            
            if (is_heap_ptr(ptr)) {
                const auto heap_ptr = unset_heap_bit(ptr);
                for (std::size_t i = 0; i != op.size; ++i) {
                    ctx.stack.push_back(ctx.heap[heap_ptr + i]);
                }
            }
            else if (is_rom_ptr(ptr)) {
                const auto rom_ptr = unset_rom_bit(ptr);
                for (std::size_t i = 0; i != op.size; ++i) {
                    ctx.stack.push_back(ctx.rom[rom_ptr + i]);
                }
            }
            else {
                for (std::size_t i = 0; i != op.size; ++i) {
                    ctx.stack.push_back(ctx.stack[ptr + i]);
                }
            }

            ++ctx.prog_ptr;
        },
        [&](op_save op) {
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);

            if (is_heap_ptr(ptr)) {
                const auto heap_ptr = unset_heap_bit(ptr);
                std::memcpy(&ctx.heap[heap_ptr], &ctx.stack[ctx.stack.size() - op.size], op.size);
                ctx.stack.resize(ctx.stack.size() - op.size);
            }
            else if (is_rom_ptr(ptr)) {
                runtime_error("cannot assign into read only memory");
            }
            else {
                runtime_assert(ptr + op.size <= ctx.stack.size(), "tried to access invalid memory address {}", ptr);
                if (ptr + op.size < ctx.stack.size()) {
                    std::memcpy(&ctx.stack[ptr], &ctx.stack[ctx.stack.size() - op.size], op.size);
                    ctx.stack.resize(ctx.stack.size() - op.size);
                }
            }

            ++ctx.prog_ptr;
        },
        [&](op_pop op) {
            ctx.stack.resize(ctx.stack.size() - op.size);
            ++ctx.prog_ptr;
        },
        [&](op_alloc_span op) {
            const auto count = pop_value<std::uint64_t>(ctx.stack);
            const auto ptr = ctx.allocator.allocate(count * op.type_size);
            push_value(ctx.stack, set_heap_bit(ptr));
            ++ctx.prog_ptr;
        },
        [&](op_dealloc_span op) {
            const auto count = pop_value<std::uint64_t>(ctx.stack);
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            runtime_assert(is_heap_ptr(ptr), "cannot delete a span to stack memory\n");
            ctx.allocator.deallocate(unset_heap_bit(ptr), count * op.type_size);
            ++ctx.prog_ptr;
        },
        [&](op_alloc_ptr op) {
            const auto ptr = ctx.allocator.allocate(op.type_size);
            push_value(ctx.stack, set_heap_bit(ptr));
            ++ctx.prog_ptr;
        },
        [&](op_dealloc_ptr op) {
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            runtime_assert(is_heap_ptr(ptr), "cannot delete a pointer to stack memory\n");
            ctx.allocator.deallocate(unset_heap_bit(ptr), op.type_size);
            ++ctx.prog_ptr;
        },
        [&](op_jump op) {
            ctx.prog_ptr = op.jump;
        },
        [&](op_jump_if_false op) {
            if (pop_value<bool>(ctx.stack)) {
                ++ctx.prog_ptr;
            } else {
                ctx.prog_ptr = op.jump;
            }
        },
        [&](op_return op) {
            const auto prev_base_ptr = read_value<std::uint64_t>(ctx.stack, ctx.base_ptr);
            const auto prev_prog_ptr = read_value<std::uint64_t>(ctx.stack, ctx.base_ptr + sizeof(std::uint64_t));
            
            std::memcpy(&ctx.stack[ctx.base_ptr], &ctx.stack[ctx.stack.size() - op.size], op.size);
            ctx.stack.resize(ctx.base_ptr + op.size);
            ctx.base_ptr = prev_base_ptr;
            ctx.prog_ptr = prev_prog_ptr;
        },
        [&](const op_function_call& op) {
            // Store the old base_ptr and prog_ptr so that they can be restored at the end of
            // the function.
            const auto new_base_ptr = ctx.stack.size() - op.args_size;
            write_value(ctx.stack, new_base_ptr, ctx.base_ptr);
            write_value(ctx.stack, new_base_ptr + sizeof(std::uint64_t), ctx.prog_ptr + 1); // Pos after function call
            
            ctx.base_ptr = new_base_ptr;
            ctx.prog_ptr = op.ptr; // Jump into the function
        },
        [&](op_call op) {
            const auto ptr = pop_value<std::uint64_t>(ctx.stack);
            // Store the old base_ptr and prog_ptr so that they can be restored at the end of
            // the function.
            const auto new_base_ptr = ctx.stack.size() - op.args_size;
            write_value(ctx.stack, new_base_ptr, ctx.base_ptr);
            write_value(ctx.stack, new_base_ptr + sizeof(std::uint64_t), ctx.prog_ptr + 1); // Pos after function call
            
            ctx.base_ptr = new_base_ptr;
            ctx.prog_ptr = ptr; // Jump into the function
        },
        [&](const op_builtin_call& op) {
            op.ptr(ctx);
            ++ctx.prog_ptr;
        },
        [&](const op_debug& op) {
            print(op.message);
            ++ctx.prog_ptr;
        },
        [&](const op_assert& op) {
            if (!pop_value<bool>(ctx.stack)) {
                runtime_error(op.message);
            }
            ++ctx.prog_ptr;
        },
        [&](op_char_eq) { binary_op<char, std::equal_to>(ctx); },
        [&](op_char_ne) { binary_op<char, std::not_equal_to>(ctx); },

        [&](op_i32_add) { binary_op<std::int32_t, std::plus>(ctx); },
        [&](op_i32_sub) { binary_op<std::int32_t, std::minus>(ctx); },
        [&](op_i32_mul) { binary_op<std::int32_t, std::multiplies>(ctx); },
        [&](op_i32_div) { binary_op<std::int32_t, std::divides>(ctx); },
        [&](op_i32_mod) { binary_op<std::int32_t, std::modulus>(ctx); },
        [&](op_i32_eq)  { binary_op<std::int32_t, std::equal_to>(ctx); },
        [&](op_i32_ne)  { binary_op<std::int32_t, std::not_equal_to>(ctx); },
        [&](op_i32_lt)  { binary_op<std::int32_t, std::less>(ctx); },
        [&](op_i32_le)  { binary_op<std::int32_t, std::less_equal>(ctx); },
        [&](op_i32_gt)  { binary_op<std::int32_t, std::greater>(ctx); },
        [&](op_i32_ge)  { binary_op<std::int32_t, std::greater_equal>(ctx); },

        [&](op_i64_add) { binary_op<std::int64_t, std::plus>(ctx); },
        [&](op_i64_sub) { binary_op<std::int64_t, std::minus>(ctx); },
        [&](op_i64_mul) { binary_op<std::int64_t, std::multiplies>(ctx); },
        [&](op_i64_div) { binary_op<std::int64_t, std::divides>(ctx); },
        [&](op_i64_mod) { binary_op<std::int64_t, std::modulus>(ctx); },
        [&](op_i64_eq)  { binary_op<std::int64_t, std::equal_to>(ctx); },
        [&](op_i64_ne)  { binary_op<std::int64_t, std::not_equal_to>(ctx); },
        [&](op_i64_lt)  { binary_op<std::int64_t, std::less>(ctx); },
        [&](op_i64_le)  { binary_op<std::int64_t, std::less_equal>(ctx); },
        [&](op_i64_gt)  { binary_op<std::int64_t, std::greater>(ctx); },
        [&](op_i64_ge)  { binary_op<std::int64_t, std::greater_equal>(ctx); },

        [&](op_u64_add) { binary_op<std::uint64_t, std::plus>(ctx); },
        [&](op_u64_sub) { binary_op<std::uint64_t, std::minus>(ctx); },
        [&](op_u64_mul) { binary_op<std::uint64_t, std::multiplies>(ctx); },
        [&](op_u64_div) { binary_op<std::uint64_t, std::divides>(ctx); },
        [&](op_u64_mod) { binary_op<std::uint64_t, std::modulus>(ctx); },
        [&](op_u64_eq)  { binary_op<std::uint64_t, std::equal_to>(ctx); },
        [&](op_u64_ne)  { binary_op<std::uint64_t, std::not_equal_to>(ctx); },
        [&](op_u64_lt)  { binary_op<std::uint64_t, std::less>(ctx); },
        [&](op_u64_le)  { binary_op<std::uint64_t, std::less_equal>(ctx); },
        [&](op_u64_gt)  { binary_op<std::uint64_t, std::greater>(ctx); },
        [&](op_u64_ge)  { binary_op<std::uint64_t, std::greater_equal>(ctx); },

        [&](op_f64_add) { binary_op<double, std::plus>(ctx); },
        [&](op_f64_sub) { binary_op<double, std::minus>(ctx); },
        [&](op_f64_mul) { binary_op<double, std::multiplies>(ctx); },
        [&](op_f64_div) { binary_op<double, std::divides>(ctx); },
        [&](op_f64_eq)  { binary_op<double, std::equal_to>(ctx); },
        [&](op_f64_ne)  { binary_op<double, std::not_equal_to>(ctx); },
        [&](op_f64_lt)  { binary_op<double, std::less>(ctx); },
        [&](op_f64_le)  { binary_op<double, std::less_equal>(ctx); },
        [&](op_f64_gt)  { binary_op<double, std::greater>(ctx); },
        [&](op_f64_ge)  { binary_op<double, std::greater_equal>(ctx); },

        [&](op_bool_and) { binary_op<bool, std::logical_and>(ctx); },
        [&](op_bool_or)  { binary_op<bool, std::logical_or>(ctx); },
        [&](op_bool_eq)  { binary_op<bool, std::equal_to>(ctx); },
        [&](op_bool_ne)  { binary_op<bool, std::not_equal_to>(ctx); },
        [&](op_bool_not) { unary_op<bool, std::logical_not>(ctx); },

        [&](op_i32_neg) { unary_op<std::int32_t, std::negate>(ctx); },
        [&](op_i64_neg) { unary_op<std::int64_t, std::negate>(ctx); },
        [&](op_f64_neg) { unary_op<double, std::negate>(ctx); }
    }, op_code);
}

auto run_program(const anzu::program& program) -> void
{
    const auto timer = scope_timer{};

    runtime_context ctx;
    ctx.rom = program.rom;
    while (ctx.prog_ptr < program.code.size()) {
        apply_op(ctx, program.code[ctx.prog_ptr]);
    }

    if (ctx.allocator.bytes_allocated() > 0) {
        anzu::print("\n -> Heap Size: {}, fix your memory leak!\n", ctx.allocator.bytes_allocated());
    }
}

auto run_program_debug(const anzu::program& program) -> void
{
    const auto timer = scope_timer{};

    runtime_context ctx;
    ctx.rom = program.rom;
    while (ctx.prog_ptr < program.code.size()) {
        const auto& op = program.code[ctx.prog_ptr];
        anzu::print("{:>4} - {}\n", ctx.prog_ptr, op);
        apply_op(ctx, program.code[ctx.prog_ptr]);
        anzu::print("Stack: {}\n", format_comma_separated(ctx.stack));
        anzu::print("Heap: allocated={}\n", ctx.allocator.bytes_allocated());
    }

    if (ctx.allocator.bytes_allocated() > 0) {
        anzu::print("\n -> Heap Size: {}, fix your memory leak!\n", ctx.allocator.bytes_allocated());
    }
}

}