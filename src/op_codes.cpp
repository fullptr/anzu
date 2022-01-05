#include "op_codes.hpp"

namespace anzu {
namespace op {

int dump::apply(anzu::stack_frame& frame) const
{
    fmt::print("{}\n", frame.pop());
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

int add::apply(anzu::stack_frame& frame) const
{
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a + b);
    return 1;
}

int sub::apply(anzu::stack_frame& frame) const
{
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a - b);
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
    auto val = frame.pop();
    return val ? 1 : jump;
}

int else_if::apply(anzu::stack_frame& frame) const
{
    return jump;
}

int end_if::apply(anzu::stack_frame& frame) const
{
    return 1;
}

}
}