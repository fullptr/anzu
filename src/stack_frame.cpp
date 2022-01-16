#include "stack_frame.hpp"
#include "print.hpp"

#include <utility>

namespace anzu {

auto frame::pop() -> anzu::object
{
    return d_values.pop();
}

auto frame::push(const anzu::object& value) -> void
{
    d_values.push(value);
}

auto frame::top(std::size_t index) -> anzu::object&
{
    return d_values.top(index);
}

auto frame::top(std::size_t index) const -> const anzu::object&
{
    return d_values.top(index);
}

auto frame::empty() const -> bool
{
    return d_values.size() == 0;
}

auto frame::fetch(const std::string& token) const -> anzu::object
{
    if (!d_symbols.contains(token)) {
        anzu::print("Error: Unknown value '{}'", token);
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
    anzu::print("Values:\n");
    for (const auto& val : d_values.all()) {
        anzu::print(" - {}\n", val);
    }
    anzu::print("Symbols:\n");
    for (const auto& [key, val] : d_symbols) {
        anzu::print(" - {} -> {}\n", key, val);
    }
}

}