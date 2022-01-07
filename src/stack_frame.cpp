#include "stack_frame.hpp"

#include <fmt/format.h>
#include <utility>

namespace anzu {

auto frame::pop() -> anzu::object
{
    auto value = d_values.back();
    d_values.pop_back();
    return value;
}

auto frame::push(const anzu::object& value) -> void
{
    d_values.push_back(value);
}

auto frame::peek() const -> anzu::object
{
    return d_values.back();
}

auto frame::empty() const -> bool
{
    return d_values.empty();
}

auto frame::fetch(const std::string& token) const -> anzu::object
{
    if (!d_symbols.contains(token)) {
        fmt::print("Error: Unknown value '{}'", token);
        std::exit(1);
    }
    return d_symbols.at(token);
}

auto frame::load(const std::string& name, const anzu::object& value) -> void
{
    d_symbols[name] = value;
}

auto frame::print() const -> void
{
    fmt::print("Values:\n");
    for (const auto& val : d_values) {
        fmt::print(" - {}\n", val);
    }
    fmt::print("Symbols:\n");
    for (const auto& [key, val] : d_symbols) {
        fmt::print(" - {} -> {}\n", key, val);
    }
}

}