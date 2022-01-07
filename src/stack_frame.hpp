#pragma once
#include "object.hpp"

#include <stack>
#include <unordered_map>
#include <string>
#include <variant>

namespace anzu {

// A small wrapper for the value stack. This will eventually handle error
// checking and checking that values are available.
class stack_frame
{
    std::vector<anzu::object>                     d_values;
    std::unordered_map<std::string, anzu::object> d_symbols;

    std::ptrdiff_t d_ptr = 0;

public:
    auto pop() -> anzu::object;
    auto push(const anzu::object& value) -> void;
    auto peek() const -> anzu::object;

    auto fetch(const std::string& name) const -> anzu::object;
    auto load(const std::string& name, const anzu::object& value) -> void;

    [[nodiscard]] auto empty() const -> bool;

    auto print() const -> void;

    std::ptrdiff_t& ptr() { return d_ptr; }
    std::ptrdiff_t ptr() const { return d_ptr; }
};

void print_value(const anzu::object& val);

}