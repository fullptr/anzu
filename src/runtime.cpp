#include "runtime.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <utility>

namespace anzu {

template <typename T>
auto pop_back(std::vector<T>& vec) -> T
{
    const auto back = vec.back();
    vec.pop_back();
    return back;   
}

template <typename T>
auto push_back(std::vector<T>& vec, const T& val) -> T&
{
    vec.push_back(val);
    return vec.back();   
}

auto program_advance(runtime_context& ctx) -> void
{
    ctx.frames.back().program_ptr += 1;
}

auto program_jump_to(runtime_context& ctx, std::intptr_t idx) -> void
{
    ctx.frames.back().program_ptr = idx;
}

auto program_ptr(const runtime_context& ctx) -> std::intptr_t
{
    return ctx.frames.back().program_ptr;
}

// Cleans up the variables used in the current frame and removes the frame
// pointers to return back to the previous scope.
auto pop_frame(runtime_context& ctx) -> void
{
    const auto num_to_pop = ctx.memory.size() - ctx.frames.back().base_ptr;
    for (std::size_t i = 0; i != num_to_pop; ++i) {
        ctx.memory.pop_back();
    }
    ctx.frames.pop_back();
}

auto apply_op(runtime_context& ctx, const op& op_code) -> void
{
    std::visit(overloaded {
        [&](const op_load_literal& op) {
            push_back(ctx.stack, op.value);
            program_advance(ctx);
        },
        [&](const op_load_local& op) {
            const auto idx = ctx.frames.back().base_ptr + op.offset;
            push_back(ctx.stack, ctx.memory[idx]);
            program_advance(ctx);
        },
        [&](const op_load_global& op) {
            const auto idx = op.position;
            push_back(ctx.stack, ctx.memory[idx]);
            program_advance(ctx);
        },
        [&](const op_pop& op) {
            pop_back(ctx.stack);
            program_advance(ctx);
        },
        [&](const op_copy_index& op) {
            const auto it = ctx.stack.rbegin() + op.index;
            push_back(ctx.stack, *it);
            program_advance(ctx);
        },
        [&](const op_save_local& op) {
            auto& frame = ctx.frames.back();
            const auto idx = frame.base_ptr + op.offset;
            if (idx >= ctx.memory.size()) {
                ctx.memory.resize(idx + 1);
            }
            ctx.memory[idx] = pop_back(ctx.stack);
            program_advance(ctx);
        },
        [&](const op_save_global& op) {
            auto& frame = ctx.frames.back();
            const auto idx = op.position;
            if (idx >= ctx.memory.size()) {
                ctx.memory.resize(idx + 1);
            }
            ctx.memory[idx] = pop_back(ctx.stack);
            program_advance(ctx);
        },
        [&](const op_if& op) {
            program_advance(ctx);
        },
        [&](const op_if_end& op) {
            program_advance(ctx);
        },
        [&](const op_else& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_while& op) {
            program_advance(ctx);
        },
        [&](const op_while_end& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_for& op) {
            program_advance(ctx);
        },
        [&](const op_for_end& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_break& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_continue& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_jump_if_false& op) {
            if (pop_back(ctx.stack).to_bool()) {
                program_advance(ctx);
            } else {
                program_jump_to(ctx, op.jump);
            }
        },
        [&](const op_function& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_function_end& op) {
            ctx.stack.push_back(null_object());
            pop_frame(ctx);
        },
        [&](const op_return& op) {
            pop_frame(ctx);
        },
        [&](const op_function_call& op) {
            program_advance(ctx); // Position after function call

            ctx.frames.emplace_back();
            auto& frame = ctx.frames.back();
            frame.base_ptr = ctx.memory.size();
            program_jump_to(ctx, op.ptr); // Jump into the function
        },
        [&](const op_builtin_call& op) {
            const auto argc = op.sig.args.size();
            auto args = std::vector<anzu::object>{};
            args.resize(argc);
            for (auto& arg : args | std::views::reverse) {
                arg = pop_back(ctx.stack);
            }

            // Call the builtin function with the given args and push the return value
            push_back(ctx.stack, op.ptr(args));
            program_advance(ctx);
        },
        [&](const op_add& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a + b);
            program_advance(ctx);
        },
        [&](const op_sub& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a - b);
            program_advance(ctx);
        },
        [&](const op_mul& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a * b);
            program_advance(ctx);
        },
        [&](const op_div& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a / b);
            program_advance(ctx);
        },
        [&](const op_mod& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a % b);
            program_advance(ctx);
        },
        [&](const op_eq& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a == b});
            program_advance(ctx);
        },
        [&](const op_ne& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a != b});
            program_advance(ctx);
        },
        [&](const op_lt& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a < b});
            program_advance(ctx);
        },
        [&](const op_le& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a <= b});
            program_advance(ctx);
        },
        [&](const op_gt& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a > b});
            program_advance(ctx);
        },
        [&](const op_ge& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a >= b});
            program_advance(ctx);
        },
        [&](const op_or& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a || b});
            program_advance(ctx);
        },
        [&](const op_and& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a && b});
            program_advance(ctx);
        },
        [&](const op_build_list& op) {
            auto list = std::make_shared<std::vector<anzu::object>>();
            for (std::size_t i = 0; i != op.size; ++i) {
                list->push_back(pop_back(ctx.stack));
            }
            push_back(ctx.stack, object{list});
            program_advance(ctx);
        },
        [&](const op_debug& op) {
            program_advance(ctx);
        }
    }, op_code);
}

auto run_program(const anzu::program& program) -> void
{
    runtime_context ctx;
    ctx.memory.reserve(1000);
    ctx.frames.emplace_back();

    while (program_ptr(ctx) < std::ssize(program)) {
        apply_op(ctx, program[program_ptr(ctx)]);
    }
}

auto run_program_debug(const anzu::program& program) -> void
{
    anzu::runtime_context ctx;
    ctx.memory.reserve(1000);
    ctx.frames.emplace_back();

    while (program_ptr(ctx) < std::ssize(program)) {
        const auto& op = program[program_ptr(ctx)];
        anzu::print("{:>4} - {}\n", program_ptr(ctx), anzu::to_string(op));
        apply_op(ctx, program[program_ptr(ctx)]);
    }
}

}