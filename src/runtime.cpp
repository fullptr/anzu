#include "runtime.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <utility>

namespace anzu {

template <typename T>
T pop_back(std::vector<T>& vec)
{
    const auto back = vec.back();
    vec.pop_back();
    return back;   
}

template <typename T>
T& push_back(std::vector<T>& vec, const T& val)
{
    vec.push_back(val);
    return vec.back();   
}

auto runtime_context::peek_frame(std::size_t index) -> frame&
{
    return frames[frames.size() - index - 1];
}

auto runtime_context::peek_value(std::size_t index) -> object&
{
    return stack[stack.size() - index - 1];
}

auto apply_op(runtime_context& ctx, const op& op_code) -> void
{
    std::visit(overloaded {
        [&](const op_load_literal& op) {
            push_back(ctx.stack, op.value);
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_load_local& op) {
            auto& frame = ctx.peek_frame();
            const auto idx = frame.base_ptr + op.offset;
            push_back(ctx.stack, ctx.memory[idx]);
            frame.program_ptr += 1;
        },
        [&](const op_load_global& op) {
            auto& frame = ctx.peek_frame();
            const auto idx = op.position;
            push_back(ctx.stack, ctx.memory[idx]);
            frame.program_ptr += 1;
        },
        [&](const op_pop& op) {
            pop_back(ctx.stack);
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_copy_index& op) {
            push_back(ctx.stack, ctx.peek_value(op.index));
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_save_local& op) {
            auto& frame = ctx.peek_frame();
            const auto idx = frame.base_ptr + op.offset;
            if (idx >= ctx.memory.size()) {
                ctx.memory.resize(idx + 1);
            }
            ctx.memory[idx] = pop_back(ctx.stack);
            frame.program_ptr += 1;
        },
        [&](const op_save_global& op) {
            auto& frame = ctx.peek_frame();
            const auto idx = op.position;
            if (idx >= ctx.memory.size()) {
                ctx.memory.resize(idx + 1);
            }
            ctx.memory[idx] = pop_back(ctx.stack);
            frame.program_ptr += 1;
        },
        [&](const op_if& op) {
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_if_end& op) {
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_else& op) {
            ctx.peek_frame().program_ptr = op.jump;
        },
        [&](const op_while& op) {
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_while_end& op) {
            ctx.peek_frame().program_ptr = op.jump;
        },
        [&](const op_for& op) {
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_for_end& op) {
            ctx.peek_frame().program_ptr = op.jump;
        },
        [&](const op_break& op) {
            ctx.peek_frame().program_ptr = op.jump;
        },
        [&](const op_continue& op) {
            ctx.peek_frame().program_ptr = op.jump;
        },
        [&](const op_jump_if_false& op) {
            if (pop_back(ctx.stack).to_bool()) {
                ctx.peek_frame().program_ptr += 1;
            } else {
                ctx.peek_frame().program_ptr = op.jump;
            }
        },
        [&](const op_function& op) {
            ctx.peek_frame().program_ptr = op.jump;
        },
        [&](const op_function_end& op) {
            const auto num_to_pop = ctx.memory.size() - ctx.peek_frame().base_ptr;
            for (std::size_t i = 0; i != num_to_pop; ++i) {
                ctx.memory.pop_back();
            }
            push_back(ctx.stack, null_object());
            ctx.frames.pop_back();
        },
        [&](const op_return& op) {
            const auto num_to_pop = ctx.memory.size() - ctx.peek_frame().base_ptr;
            for (std::size_t i = 0; i != num_to_pop; ++i) {
                ctx.memory.pop_back();
            }
            ctx.frames.pop_back();
        },
        [&](const op_function_call& op) {
            ctx.peek_frame().program_ptr += 1; // Position after function call

            ctx.frames.emplace_back();
            auto& frame = ctx.frames.back();
            frame.base_ptr = ctx.memory.size();
            frame.program_ptr = op.ptr; // Jump into the function
        },
        [&](const op_builtin_call& op) {
            const auto argc = op.sig.args.size();
            auto args = std::vector<anzu::object>{};
            args.resize(argc);
            for (std::size_t i = 0; i != argc; ++i) {
                args[argc - 1 - i] = pop_back(ctx.stack);
            }

            // Call the builtin function with the given args and push the return value
            push_back(ctx.stack, op.ptr(args));
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_add& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a + b);
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_sub& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a - b);
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_mul& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a * b);
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_div& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a / b);
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_mod& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, a % b);
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_eq& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a == b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_ne& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a != b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_lt& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a < b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_le& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a <= b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_gt& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a > b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_ge& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a >= b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_or& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a || b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_and& op) {
            auto b = pop_back(ctx.stack);
            auto a = pop_back(ctx.stack);
            push_back(ctx.stack, object{a && b});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_build_list& op) {
            auto list = std::make_shared<std::vector<anzu::object>>();
            for (std::size_t i = 0; i != op.size; ++i) {
                list->push_back(pop_back(ctx.stack));
            }
            push_back(ctx.stack, object{list});
            ctx.peek_frame().program_ptr += 1;
        },
        [&](const op_debug& op) {
            auto& frame = ctx.peek_frame();
            frame.program_ptr += 1;
        }
    }, op_code);
}

auto run_program(const anzu::program& program) -> void
{
    runtime_context ctx;
    ctx.memory.reserve(1000);
    ctx.frames.emplace_back();

    while (ctx.peek_frame().program_ptr < std::ssize(program)) {
        apply_op(ctx, program[ctx.peek_frame().program_ptr]);
    }
}

auto run_program_debug(const anzu::program& program) -> void
{
    anzu::runtime_context ctx;
    ctx.memory.reserve(1000);
    ctx.frames.emplace_back();

    while (ctx.peek_frame().program_ptr < std::ssize(program)) {
        const auto& op = program[ctx.peek_frame().program_ptr];
        anzu::print("{:>4} - {}\n", ctx.peek_frame().program_ptr, anzu::to_string(op));
        apply_op(ctx, program[ctx.peek_frame().program_ptr]);
    }
}

}