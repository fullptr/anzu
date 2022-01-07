#include "op_codes.hpp"
#include "object.hpp"

#include <iostream>
#include <string>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

}

void op_store::apply(anzu::stack_frame& frame) const
{
    frame.load(name, frame.pop());
    frame.ptr() += 1;
}

void op_dump::apply(anzu::stack_frame& frame) const
{
    fmt::print("{}\n", frame.pop());
    frame.ptr() += 1;
}

void op_pop::apply(anzu::stack_frame& frame) const
{
    frame.pop();
    frame.ptr() += 1;
}

void op_push_const::apply(anzu::stack_frame& frame) const
{
    frame.push(value);
    frame.ptr() += 1;
}

void op_push_var::apply(anzu::stack_frame& frame) const
{
    frame.push(frame.fetch(name));
    frame.ptr() += 1;
}

void op_add::apply(anzu::stack_frame& frame) const
{
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

void op_sub::apply(anzu::stack_frame& frame) const
{
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

void op_mul::apply(anzu::stack_frame& frame) const
{
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

void op_div::apply(anzu::stack_frame& frame) const
{
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

void op_mod::apply(anzu::stack_frame& frame) const
{
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

void op_dup::apply(anzu::stack_frame& frame) const
{
    frame.push(frame.peek());
    frame.ptr() += 1;
}

void op_print_frame::apply(anzu::stack_frame& frame) const
{
    frame.print();
    frame.ptr() += 1;
}

void op_eq::apply(anzu::stack_frame& frame) const
{
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

void op_ne::apply(anzu::stack_frame& frame) const
{
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

void op_lt::apply(anzu::stack_frame& frame) const
{
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

void op_le::apply(anzu::stack_frame& frame) const
{
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

void op_gt::apply(anzu::stack_frame& frame) const
{
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

void op_ge::apply(anzu::stack_frame& frame) const
{
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

void op_or::apply(anzu::stack_frame& frame) const
{
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

void op_and::apply(anzu::stack_frame& frame) const
{
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

void op_input::apply(anzu::stack_frame& frame) const
{
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

void op_if::apply(anzu::stack_frame& frame) const
{
    frame.ptr() += 1;
}

void op_end_if::apply(anzu::stack_frame& frame) const
{
    frame.ptr() += 1;
}

void op_elif::apply(anzu::stack_frame& frame) const
{
    frame.ptr() = jump;
}

void op_else::apply(anzu::stack_frame& frame) const
{
    frame.ptr() = jump;
}

void op_while::apply(anzu::stack_frame& frame) const
{
    frame.ptr() += 1;
}

void op_end_while::apply(anzu::stack_frame& frame) const
{
    frame.ptr() = jump;
}

void op_break::apply(anzu::stack_frame& frame) const
{
    frame.ptr() = jump;
}

void op_continue::apply(anzu::stack_frame& frame) const
{
    frame.ptr() = jump;
}

void op_do::apply(anzu::stack_frame& frame) const
{
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

}