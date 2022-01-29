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
    std::unordered_map<std::string, anzu::object> d_symbols;

    std::intptr_t d_ptr = 0;

public:
    auto fetch(const std::string& name) const -> anzu::object;
    auto load(const std::string& name, const anzu::object& value) -> void;
    auto print() const -> void;

    std::intptr_t& ptr() { return d_ptr; }
    std::intptr_t ptr() const { return d_ptr; }
};

class context
{
    std::vector<frame>        d_frames;
    std::vector<anzu::object> d_values;

public:
    auto push_frame() -> frame&;
    auto pop_frame() -> void;
    auto peek_frame(std::size_t index = 0) -> frame&;

    auto push_value(const object& val) -> object&;
    auto pop_value() -> object;
    auto peek_value(std::size_t index = 0) -> object&;
    auto size() const -> std::size_t;
};

}