#pragma once
#include <stack>
#include <unordered_map>
#include <string>

namespace anzu {

// A small wrapper for the value stack. This will eventually handle error
// checking and checking that values are available.
class value_stack
{
public:
    using type = int; // Will become a variant
    
private:
    std::stack<type>                      d_values;
    std::unordered_map<std::string, type> d_symbols;

public:
    auto pop() -> type;
    auto push(const type& value) -> void;
    auto peek() const -> type;

    auto fetch(const std::string& name) const -> type;
    auto load(const std::string& name, const type& value) -> void;

    [[nodiscard]] auto empty() const -> bool;
};

}