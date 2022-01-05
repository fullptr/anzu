#include "op_codes.hpp"

namespace anzu {
namespace op {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

int dump::apply(anzu::stack_frame& frame) const
{
    anzu::print_value(frame.pop());
    fmt::print("\n");
    return 1;
}

int pop::apply(anzu::stack_frame& frame) const
{
    frame.pop();
    return 1;
}

int push_int::apply(anzu::stack_frame& frame) const
{
    frame.push(value);
    return 1;
}

int store_int::apply(anzu::stack_frame& frame) const
{
    frame.load(name, value);
    return 1;
}

int push_var::apply(anzu::stack_frame& frame) const
{
    frame.push(frame.fetch(name));
    return 1;
}

int store_var::apply(anzu::stack_frame& frame) const
{
    frame.load(name, frame.fetch(source));
    return 1;
}

int push_bool::apply(anzu::stack_frame& frame) const
{
    frame.push(value);
    return 1;   
}

int store_bool::apply(anzu::stack_frame& frame) const
{
    frame.load(name, value);
    return 1;   
}

int add::apply(anzu::stack_frame& frame) const
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
    return 1;
}

int sub::apply(anzu::stack_frame& frame) const
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
    return 1;
}

int dup::apply(anzu::stack_frame& frame) const
{
    frame.push(frame.peek());
    return 1;
}

int print_frame::apply(anzu::stack_frame& frame) const
{
    frame.print();
    return 1;
}

int begin_if::apply(anzu::stack_frame& frame) const
{
    auto condition = std::visit(overloaded {
        [](int v) { return v != 0; },
        [](bool v) { return v; }
    }, frame.pop());
    return condition ? 1 : jump;
}

int else_if::apply(anzu::stack_frame& frame) const
{
    return jump;
}

int end_if::apply(anzu::stack_frame& frame) const
{
    return 1;
}

int equals::apply(anzu::stack_frame& frame) const
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
    return 1;
}

}
}