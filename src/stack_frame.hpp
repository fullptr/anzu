#pragma once
#include <stack>
#include <unordered_map>
#include <string>
#include <variant>

namespace anzu {

// A small wrapper for the value stack. This will eventually handle error
// checking and checking that values are available.
class stack_frame
{
public:
    using type = std::variant<int, bool>;
    
private:
    std::vector<type>                     d_values;
    std::unordered_map<std::string, type> d_symbols;

public:
    auto pop() -> type;
    auto push(const type& value) -> void;
    auto peek() const -> type;

    auto has(const std::string& name) const -> bool;
    auto fetch(const std::string& name) const -> type;
    auto load(const std::string& name, const type& value) -> void;

    [[nodiscard]] auto empty() const -> bool;

    auto print() const -> void;
};

void print_value(const stack_frame::type& val);

}