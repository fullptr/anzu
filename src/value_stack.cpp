#include "value_stack.hpp"

namespace anzu {

auto value_stack::pop() -> type
{
    auto value = d_values.top();
    d_values.pop();
    return value;
}

auto value_stack::push(const type& value) -> void
{
    d_values.push(value);
}

auto value_stack::empty() const -> bool
{
    return d_values.empty();
}

}