#pragma once
#include "object.hpp"

#include <stack>
#include <unordered_map>
#include <string>
#include <variant>
#include <ranges>

namespace anzu {

// A small wrapper for the value stack. This will eventually handle error
// checking and checking that values are available.
class frame
{
    std::vector<anzu::object>                     d_values;
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
    std::vector<frame> d_frames;

public:
    auto push_frame() -> frame&;
    auto pop_frame() -> void;
    auto peek_frame(std::size_t index = 0) -> frame&;
};

}