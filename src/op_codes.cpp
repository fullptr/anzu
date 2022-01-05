#include "op_codes.hpp"

namespace anzu {
namespace op {

void dump::apply(anzu::stack_frame& frame) const
{
    fmt::print("{}\n", frame.pop());
}

void pop::apply(anzu::stack_frame& frame) const
{
    frame.pop();
}

void push_int::apply(anzu::stack_frame& frame) const
{
    frame.push(value);
}

void store_int::apply(anzu::stack_frame& frame) const
{
    frame.load(name, value);
}

void push_var::apply(anzu::stack_frame& frame) const
{
    frame.push(frame.fetch(name));
}

void store_var::apply(anzu::stack_frame& frame) const
{
    frame.load(name, frame.fetch(source));
}

void add::apply(anzu::stack_frame& frame) const
{
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a + b);
}

void sub::apply(anzu::stack_frame& frame) const
{
    auto b = frame.pop();
    auto a = frame.pop();
    frame.push(a - b);
}

void dup::apply(anzu::stack_frame& frame) const
{
    frame.push(frame.peek());
}

void print_frame::apply(anzu::stack_frame& frame) const
{
    frame.print();
}

void begin_if::apply(anzu::stack_frame& frame) const
{
}

void end_if::apply(anzu::stack_frame& frame) const
{
}

}
}