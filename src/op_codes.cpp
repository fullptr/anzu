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

void verify_stack(const anzu::frame& frame, int count, std::string_view name)
{
    const auto type = count != 1 ? "args" : "arg";
    const auto err = "stack underflow: '{}' requires {} {}";
    anzu::verify(frame.stack_size() >= count, err, name, count, type);
}

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

void transfer_values(anzu::frame& src, anzu::frame& dst, int count)
{
    anzu::verify(src.stack_size() >= count, "could not transfer {} args\n", count);
    for (int i = 0; i != count; ++i) { dst.push(src.top(count - 1 - i)); }
    for (int i = 0; i != count; ++i) { src.pop(); }
}

}

void op_push_const::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.push(value);
    frame.ptr() += 1;
}

void op_push_var::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.push(frame.fetch(name));
    frame.ptr() += 1;
}

void op_pop::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.pop();
    frame.ptr() += 1;
}

void op_dup::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.push(frame.top());
    frame.ptr() += 1;
}

void op_swap::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "swap");
    swap(frame.top(0), frame.top(1));
    frame.ptr() += 1;
}

void op_rot::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 3, "rot");
    swap(frame.top(0), frame.top(1));
    swap(frame.top(1), frame.top(2));
    frame.ptr() += 1;
}

void op_over::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "over");
    frame.push(frame.top(1));
    frame.ptr() += 1;
}

void op_store::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 1, "store");
    frame.load(name, frame.pop());
    frame.ptr() += 1;
}

void op_if::apply(anzu::context& ctx) const
{
    ctx.top().ptr() += 1;
}

void op_if_end::apply(anzu::context& ctx) const
{
    ctx.top().ptr() += 1;
}

void op_else::apply(anzu::context& ctx) const
{
    ctx.top().ptr() = jump;
}

void op_while::apply(anzu::context& ctx) const
{
    ctx.top().ptr() += 1;
}

void op_while_end::apply(anzu::context& ctx) const
{
    ctx.top().ptr() = jump;
}

void op_break::apply(anzu::context& ctx) const
{
    ctx.top().ptr() = jump;
}

void op_continue::apply(anzu::context& ctx) const
{
    ctx.top().ptr() = jump;
}

void op_do::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    if (frame.pop().to_bool()) {
        frame.ptr() += 1;
    } else {
        frame.ptr() = jump;
    }
}

void op_function::apply(anzu::context& ctx) const
{
    ctx.top().ptr() = jump;
}

void op_function_call::apply(anzu::context& ctx) const
{
    auto& curr = ctx.push({}); // New frame
    auto& prev = ctx.top(1);   // One under the top

    curr.ptr() = jump; // Jump into the function
    prev.ptr() += 1;   // The position in the program where it will resume

    transfer_values(prev, curr, argc);
}

void op_function_end::apply(anzu::context& ctx) const
{
    transfer_values(ctx.top(0), ctx.top(1), retc);
    ctx.pop(); // Remove stack frame
}

void op_return::apply(anzu::context& ctx) const
{
    transfer_values(ctx.top(0), ctx.top(1), retc);
    ctx.pop(); // Remove stack frame
}

void op_builtin_function_call::apply(anzu::context& ctx) const
{
    func(ctx);
    ctx.top().ptr() += 1;
}

template <typename A, typename B>
concept addable = requires(A a, B b) { { a + b }; };

void op_add::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "+");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a + b);
    frame.ptr() += 1;
}

template <typename A, typename B>
concept subtractable = requires(A a, B b) { { a - b }; };

void op_sub::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "-");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a - b);
    frame.ptr() += 1;
}

template <typename A, typename B>
concept multipliable = requires(A a, B b) { { a * b }; };

void op_mul::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "*");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a * b);
    frame.ptr() += 1;
}

void op_div::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "/");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a / b);
    frame.ptr() += 1;
}

void op_mod::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "%");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a % b);
    frame.ptr() += 1;
}

void op_eq::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "==");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a == b);
    frame.ptr() += 1;
}

void op_ne::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "!=");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a != b);
    frame.ptr() += 1;
}

void op_lt::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "<");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a < b);
    frame.ptr() += 1;
}

void op_le::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "<=");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a <= b);
    frame.ptr() += 1;
}

void op_gt::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, ">");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a >b);
    frame.ptr() += 1;
}

void op_ge::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, ">=");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a >= b);
    frame.ptr() += 1;
}

void op_or::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "or");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a || b);
    frame.ptr() += 1;
}

void op_and::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 2, "and");
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a && b);
    frame.ptr() += 1;
}

void op_to_int::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.push(frame.pop().to_int());
    frame.ptr() += 1;
}

void op_to_bool::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.push(frame.pop().to_bool());
    frame.ptr() += 1;
}

void op_to_str::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.push(frame.pop().to_str());
    frame.ptr() += 1;
}

void op_input::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    std::string in;
    std::cin >> in;
    frame.push(in);
    frame.ptr() += 1;
}

void op_dump::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    anzu::verify_stack(frame, 1, ".");
    anzu::print("{}", frame.pop());
    frame.ptr() += 1;
}

}