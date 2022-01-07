#include "stack_frame.hpp"

#include <fmt/format.h>
#include <utility>

namespace anzu {

auto stack_frame::pop() -> anzu::object
{
    auto value = d_values.back();
    d_values.pop_back();
    return value;
}

auto stack_frame::push(const anzu::object& value) -> void
{
    d_values.push_back(value);
}

auto stack_frame::peek() const -> anzu::object
{
    return d_values.back();
}

auto stack_frame::empty() const -> bool
{
    return d_values.empty();
}

auto stack_frame::fetch(const std::string& token) const -> anzu::object
{
    if (!d_symbols.contains(token)) {
        fmt::print("Error: Unknown value '{}'", token);
        std::exit(1);
    }
    return d_symbols.at(token);
}

auto stack_frame::load(const std::string& name, const anzu::object& value) -> void
{
    d_symbols[name] = value;
}

auto stack_frame::print() const -> void
{
    fmt::print("Values:\n");
    for (const auto& val : d_values) {
        fmt::print(" - ");
        anzu::print_value(val);
        fmt::print("\n");
    }
    fmt::print("Symbols:\n");
    for (const auto& [key, val] : d_symbols) {
        fmt::print(" - {} -> ", key);
        anzu::print_value(val);
        fmt::print("\n");
    }
}

void print_value(const anzu::object& val)
{
    std::visit([](const auto& v) { fmt::print("{}", v); }, val);
}

}