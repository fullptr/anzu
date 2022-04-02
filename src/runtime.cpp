#include "runtime.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/scope_timer.hpp"

#include <chrono>
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

template <typename T>
auto pop_back(std::vector<T>& vec) -> T
{
    const auto back = vec.back();
    vec.pop_back();
    return back;   
}

auto program_advance(runtime_context& ctx) -> void
{
    ctx.frames.back().program_ptr += 1;
}

auto program_jump_to(runtime_context& ctx, std::size_t idx) -> void
{
    ctx.frames.back().program_ptr = idx;
}

auto program_ptr(const runtime_context& ctx) -> std::size_t
{
    return ctx.frames.back().program_ptr;
}

auto base_ptr(const runtime_context& ctx) -> std::size_t
{
    return ctx.frames.back().base_ptr;
}

auto save_top_at(runtime_context& ctx, std::size_t idx, std::size_t size) -> void
{
    runtime_assert(idx + size <= ctx.memory.size(), "tried to access invalid memory address {}", idx);
    if (idx == ctx.memory.size() - size) {
        return;
    }
    for (std::size_t i = 0; i != size; ++i) {
        ctx.memory[idx + i] = ctx.memory[ctx.memory.size() - size + i];
    }
    for (std::size_t i = 0; i != size; ++i) {
        ctx.memory.pop_back();
    }
}

// Cleans up the variables used in the current frame and removes the frame
// pointers to return back to the previous scope.
auto pop_frame(runtime_context& ctx) -> void
{
    const auto return_size = ctx.frames.back().return_size;

    for (std::size_t i = 0; i != return_size; ++i) {
        ctx.memory[base_ptr(ctx) + i] = ctx.memory[ctx.memory.size() - return_size + i];
    }
    while (ctx.memory.size() > base_ptr(ctx) + return_size) {
        ctx.memory.pop_back();
    }
    ctx.frames.pop_back();
}

auto apply_op(runtime_context& ctx, const op& op_code) -> void
{
    std::visit(overloaded {
        [&](const op_load_literal& op) {
            for (const auto& block : op.value) {
                ctx.memory.push_back(block);
            }
            program_advance(ctx);
        },
        [&](const op_push_global_addr& op) {
            const auto idx = op.position;
            const auto ptr = block_ptr{ .ptr=idx, .size=op.size };
            ctx.memory.push_back(ptr);
            program_advance(ctx);
        },
        [&](const op_push_local_addr& op) {
            const auto idx = base_ptr(ctx) + op.offset;
            const auto ptr = block_ptr{ .ptr=idx, .size=op.size };
            ctx.memory.push_back(ptr);
            program_advance(ctx);
        },
        [&](op_modify_ptr) {
            const auto size = std::get<block_int>(pop_back(ctx.memory));
            const auto offset = std::get<block_int>(pop_back(ctx.memory));
            auto& ptr = std::get<block_ptr>(ctx.memory.back());
            ptr.ptr += offset;
            ptr.size = size;
            program_advance(ctx);
        },
        [&](op_load) {
            const auto ptr_blk = pop_back(ctx.memory);
            const auto ptr = std::get<block_ptr>(ptr_blk);
            for (std::size_t i = 0; i != ptr.size; ++i) {
                ctx.memory.push_back(ctx.memory[ptr.ptr + i]);
            }
            program_advance(ctx);
        },
        [&](op_save) {
            const auto ptr_blk = pop_back(ctx.memory);
            const auto ptr = std::get<block_ptr>(ptr_blk);
            save_top_at(ctx, ptr.ptr, ptr.size);
            program_advance(ctx);
        },
        [&](const op_pop& op) {
            for (std::size_t i = 0; i != op.size; ++i) {
                ctx.memory.pop_back();
            }
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
        [&](const op_loop_begin& op) {
            program_advance(ctx);
        },
        [&](const op_loop_end& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_break& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_continue& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_jump_if_false& op) {
            if (std::get<block_bool>(ctx.memory.back())) {
                program_advance(ctx);
            } else {
                program_jump_to(ctx, op.jump);
            }
            ctx.memory.pop_back();
        },
        [&](const op_function& op) {
            program_jump_to(ctx, op.jump);
        },
        [&](const op_function_end& op) {
            const auto null_return = make_null();
            for (const auto& block : null_return.data) {
                ctx.memory.push_back(block);
            }
            pop_frame(ctx);
        },
        [&](const op_return& op) {
            pop_frame(ctx);
        },
        [&](const op_function_call& op) {
            program_advance(ctx); // Position after function call

            ctx.frames.emplace_back();
            ctx.frames.back().base_ptr = ctx.memory.size() - op.args_size;
            ctx.frames.back().return_size = op.return_size;
            program_jump_to(ctx, op.ptr); // Jump into the function
        },
        [&](const op_builtin_call& op) {
            auto args = std::vector<anzu::block>(op.args_size);
            for (auto& arg : args | std::views::reverse) {
                arg = pop_back(ctx.memory);
            }

            ctx.memory.push_back(op.ptr(args));
            program_advance(ctx);
        },
        [&](const op_builtin_mem_op& op) {
            op.ptr(ctx.memory);
            program_advance(ctx);
        }
    }, op_code);
}

auto run_program(const anzu::program& program) -> void
{
    const auto timer = scope_timer{};

    runtime_context ctx;
    ctx.frames.emplace_back();
    while (program_ptr(ctx) < program.size()) {
        apply_op(ctx, program[program_ptr(ctx)]);
    }
}

auto run_program_debug(const anzu::program& program) -> void
{
    const auto timer = scope_timer{};

    runtime_context ctx;
    ctx.frames.emplace_back();
    while (program_ptr(ctx) < program.size()) {
        const auto& op = program[program_ptr(ctx)];
        anzu::print("{:>4} - {}\n", program_ptr(ctx), op);
        apply_op(ctx, program[program_ptr(ctx)]);
        anzu::print("Memory: {}\n", format_comma_separated(ctx.memory));
    }
}

}