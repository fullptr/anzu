#pragma once
#include <stack>

namespace anzu {

// A small wrapper for the value stack. This will eventually handle error
// checking and checking that values are available.
class value_stack
{
public:
    using type = int; // Will become a variant
    
private:
    std::stack<type> d_values;

public:
    auto pop() -> type;
    auto push(const type& value) -> void;

    [[nodiscard]] auto empty() const -> bool;
};

}