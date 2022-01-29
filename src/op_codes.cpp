#include "op_codes.hpp"
#include "object.hpp"
#include "print.hpp"
#include "functions.hpp"

#include <iostream>
#include <string>

namespace anzu {
namespace {

template <typename... Args>
void verify(bool condition, std::string_view msg, Args&&... args)
{
    if (!condition) {
        anzu::print(msg, std::forward<Args>(args)...);
        std::exit(1);
    }
}

void verify_stack(const context& ctx, int count, std::string_view name)
{
    const auto type = count != 1 ? "args" : "arg";
    const auto err = "stack underflow: '{}' requires {} {}";
    anzu::verify(ctx.size() >= count, err, name, count, type);
}

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

}

void op_push_const::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    ctx.push_value(value);
    frame.ptr += 1;
}

void op_push_var::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    ctx.push_value(frame.memory.get(name));
    frame.ptr += 1;
}

void op_pop::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    ctx.pop_value();
    frame.ptr += 1;
}

void op_copy_index::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, index + 1, "copy_index");
    ctx.push_value(ctx.peek_value(index));
    frame.ptr += 1;
}


void op_store::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 1, "store");
    frame.memory.insert(name, ctx.pop_value());
    frame.ptr += 1;
}

void op_if::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr += 1;
}

void op_if_end::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr += 1;
}

void op_else::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr = jump;
}

void op_while::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr += 1;
}

void op_while_end::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr = jump;
}

void op_for::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr += 1;
}

void op_for_end::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr = jump;
}

void op_break::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr = jump;
}

void op_continue::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr = jump;
}

void op_jump_if_false::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    if (ctx.pop_value().to_bool()) {
        frame.ptr += 1;
    } else {
        frame.ptr = jump;
    }
}

void op_function::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr = jump;
}

void op_function_end::apply(anzu::context& ctx) const
{
    ctx.push_value(anzu::null_object());
    ctx.pop_frame();
}

void op_return::apply(anzu::context& ctx) const
{
    ctx.pop_frame();
}

void op_function_call::apply(anzu::context& ctx) const
{
    ctx.peek_frame().ptr += 1; // Position after function call

    auto& frame = ctx.push_frame(); 
    frame.ptr = ptr; // Jump into the function

    // Pop elements off the stack and load them into the new scope
    for (const auto& arg_name : std::views::reverse(arg_names)) {
        frame.memory.insert(arg_name, ctx.pop_value());
    }
}

void op_builtin_call::apply(anzu::context& ctx) const
{
    func(ctx);
    ctx.peek_frame().ptr += 1;
}

template <typename A, typename B>
concept addable = requires(A a, B b) { { a + b }; };

void op_add::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "+");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a + b);
    frame.ptr += 1;
}

template <typename A, typename B>
concept subtractable = requires(A a, B b) { { a - b }; };

void op_sub::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "-");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a - b);
    frame.ptr += 1;
}

template <typename A, typename B>
concept multipliable = requires(A a, B b) { { a * b }; };

void op_mul::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "*");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a * b);
    frame.ptr += 1;
}

void op_div::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "/");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a / b);
    frame.ptr += 1;
}

void op_mod::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "%");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a % b);
    frame.ptr += 1;
}

void op_eq::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "==");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a == b);
    frame.ptr += 1;
}

void op_ne::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "!=");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a != b);
    frame.ptr += 1;
}

void op_lt::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "<");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a < b);
    frame.ptr += 1;
}

void op_le::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "<=");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a <= b);
    frame.ptr += 1;
}

void op_gt::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, ">");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a > b);
    frame.ptr += 1;
}

void op_ge::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, ">=");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a >= b);
    frame.ptr += 1;
}

void op_or::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "or");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a || b);
    frame.ptr += 1;
}

void op_and::apply(anzu::context& ctx) const
{
    auto& frame = ctx.peek_frame();
    verify_stack(ctx, 2, "and");
    auto b = ctx.pop_value();
    auto a = ctx.pop_value();
    ctx.push_value(a && b);
    frame.ptr += 1;
}

}