#include "op_codes.hpp"
#include "object.hpp"

#include <iostream>
#include <string>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

void transfer_values(anzu::frame& src, anzu::frame& dst, int count)
{
    for (int i = 0; i != count; ++i) { dst.push(src.top(count - 1 - i)); }
    for (int i = 0; i != count; ++i) { src.pop(); }
}

}

void op_store::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.load(name, frame.pop());
    frame.ptr() += 1;
}

void op_dump::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    fmt::print("{}\n", frame.pop());
    frame.ptr() += 1;
}

void op_pop::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.pop();
    frame.ptr() += 1;
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

void op_add::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, int> && std::is_same_v<B, int>) {
            frame.push(a + b);
        } else {
            fmt::print("Can only add integers\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_sub::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, int> && std::is_same_v<B, int>) {
            frame.push(a - b);
        } else {
            fmt::print("Can only sub integers\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_mul::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, int> && std::is_same_v<B, int>) {
            frame.push(a * b);
        } else {
            fmt::print("Can only add integers\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_div::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, int> && std::is_same_v<B, int>) {
            frame.push(a / b);
        } else {
            fmt::print("Can only sub integers\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_mod::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, int> && std::is_same_v<B, int>) {
            frame.push(a % b);
        } else {
            fmt::print("Can only sub integers\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_dup::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.push(frame.top());
    frame.ptr() += 1;
}

void op_print_frame::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    frame.print();
    frame.ptr() += 1;
}

void op_eq::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, B>) {
            frame.push(a == b);
        } else {
            fmt::print("Can only compare values of the same type\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_ne::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, B>) {
            frame.push(a != b);
        } else {
            fmt::print("Can only compare values of the same type\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_lt::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, B>) {
            frame.push(a < b);
        } else {
            fmt::print("Can only compare values of the same type\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_le::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, B>) {
            frame.push(a <= b);
        } else {
            fmt::print("Can only compare values of the same type\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_gt::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, B>) {
            frame.push(a > b);
        } else {
            fmt::print("Can only compare values of the same type\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_ge::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, B>) {
            frame.push(a >= b);
        } else {
            fmt::print("Can only compare values of the same type\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_or::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, bool> && std::is_same_v<B, bool>) {
            frame.push(a || b);
        } else {
            fmt::print("Logical OR can only be used on bools\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_and::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    auto b = frame.pop();
    auto a = frame.pop();
    std::visit([&]<typename A, typename B>(const A& a, const B& b) {
        if constexpr (std::is_same_v<A, bool> && std::is_same_v<B, bool>) {
            frame.push(a && b);
        } else {
            fmt::print("Logical AND can only be used on bools\n");
            std::exit(1);
        }
    }, a, b);
    frame.ptr() += 1;
}

void op_input::apply(anzu::context& ctx) const
{
    auto& frame = ctx.top();
    fmt::print("Input: ");
    std::string in;
    std::cin >> in;
    if (!anzu::is_literal(in)) {
        fmt::print("[BAD INPUT]\n");
        std::exit(1);
    }
    frame.push(anzu::parse_literal(in));
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

void op_elif::apply(anzu::context& ctx) const
{
    ctx.top().ptr() = jump;
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
    auto condition = std::visit(overloaded {
        [](int v) { return v != 0; },
        [](bool v) { return v; }
    }, frame.pop());

    if (condition) {
        frame.ptr() += 1;
    } else {
        frame.ptr() = jump;
    }
}

void op_function::apply(anzu::context& ctx) const
{
    ctx.top().ptr() = jump;
}

void op_function_end::apply(anzu::context& ctx) const
{
    // Move the required number of arguments back to the underlying frame
    transfer_values(ctx.top(0), ctx.top(1), retc);
    
    ctx.pop(); // Remove stack frame
}

void op_function_call::apply(anzu::context& ctx) const
{
    auto& curr = ctx.push({}); // New frame
    auto& prev = ctx.top(1);   // One under the top

    curr.ptr() = jump; // Jump into the function
    prev.ptr() += 1;   // The position in the program where it will resume

    // Move the required number of arguments over to the new frame
    transfer_values(prev, curr, argc);
}

void op_return::apply(anzu::context& ctx) const
{
    // Move the required number of arguments back to the underlying frame
    transfer_values(ctx.top(0), ctx.top(1), retc);

    ctx.pop(); // Remove stack frame
}

}