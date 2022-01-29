#include "stack_frame.hpp"
#include "print.hpp"

#include <utility>

namespace anzu {

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
    anzu::print("Frame:\n");
    for (const auto& [key, val] : d_symbols) {
        anzu::print(" - {} -> {}\n", key, val);
    }
}

auto context::push_frame() -> frame&
{
    d_frames.push_back({});
    return d_frames.back();
}

auto context::pop_frame() -> void
{
    d_frames.pop_back();
}

auto context::peek_frame(std::size_t index) -> frame&
{
    return d_frames[d_frames.size() - index - 1];
}

auto context::push_value(const object& val) -> object&
{
    d_values.push_back(val);
    return d_values.back();
}

auto context::pop_value() -> object
{
    auto ret = d_values.back();
    d_values.pop_back();
    return ret;
}

auto context::peek_value(std::size_t index) -> object&
{
    return d_values[d_values.size() - index - 1];
}

auto context::size() const -> std::size_t
{
    return d_values.size();
}

}