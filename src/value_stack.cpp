#include "value_stack.hpp"

#include <fmt/format.h>
#include <utility>

namespace anzu {
namespace {

bool is_int(const std::string& token)
{
    return token.find_first_not_of("0123456789") == std::string::npos;
}

}

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

auto value_stack::peek() const -> type
{
    return d_values.top();
}

auto value_stack::empty() const -> bool
{
    return d_values.empty();
}

auto value_stack::fetch(const std::string& token) const -> type
{
    if (!d_symbols.contains(token)) {
        fmt::print("Error: Unknown int");
        std::exit(1);
    }
    return d_symbols.at(token);
}

auto value_stack::load(const std::string& name, const type& value) -> void
{
    d_symbols[name] = value;
}

}