#pragma once
#include "object.hpp"

#include <stack>
#include <unordered_map>
#include <string>
#include <variant>
#include <ranges>

namespace anzu {

template <typename T>
class stack
{
    std::vector<T> d_values;

public:
    auto pop() -> T
    {
        T val = d_values.back();
        d_values.pop_back();
        return val;
    }

    auto push(const T& value) -> T&
    {
        d_values.push_back(value);
        return d_values.back();
    }

    auto top(std::size_t index = 0) const -> const T&
    {
        return d_values[d_values.size() - index - 1];
    }

    auto top(std::size_t index = 0) -> T&
    {
        return d_values[d_values.size() - index - 1];
    }

    auto size() const -> std::size_t
    {
        return d_values.size();
    }

    auto all() const { return d_values | std::views::reverse; }
};

// A small wrapper for the value stack. This will eventually handle error
// checking and checking that values are available.
class frame
{
    anzu::stack<anzu::object>                     d_values;
    std::unordered_map<std::string, anzu::object> d_symbols;

    std::intptr_t d_ptr = 0;

public:
    auto pop() -> anzu::object;
    auto push(const anzu::object& value) -> void;
    auto top(std::size_t index = 0) -> anzu::object&;
    auto top(std::size_t index = 0) const -> const anzu::object&;
    auto stack_size() const -> std::size_t { return d_values.size(); }

    auto fetch(const std::string& name) const -> anzu::object;
    auto load(const std::string& name, const anzu::object& value) -> void;

    [[nodiscard]] auto empty() const -> bool;

    auto print() const -> void;

    std::intptr_t& ptr() { return d_ptr; }
    std::intptr_t ptr() const { return d_ptr; }
};

class context
{
    anzu::stack<frame> d_frames;

public:
    auto push() -> frame& { return d_frames.push({}); }
    auto pop() -> frame { return d_frames.pop(); }
    auto top(std::size_t index = 0) -> frame& { return d_frames.top(index); }
};

}