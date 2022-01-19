#pragma once
#include <format>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "print.hpp"

namespace anzu {

class object;
using object_list = std::shared_ptr<std::vector<object>>;

class object
{
    using value_type = std::variant<
        int,
        bool,
        std::string,
        object_list
    >;

    value_type d_value;

public:
    template <typename Obj>
    object(const Obj& obj) : d_value{obj} {}
    
    object() : d_value{0} {}

    auto is_int() const -> bool;
    auto is_bool() const -> bool;
    auto is_str() const -> bool;
    auto is_list() const -> bool;

    // Casts, for certain types, converts the object to the requested type.
    auto to_int() const -> int;
    auto to_bool() const -> bool;
    auto to_str() const -> std::string;
    
    template <typename T>
    auto as() -> T&
    {
        if (std::holds_alternative<T>(d_value)) {
            return std::get<T>(d_value);
        }
        anzu::print("error: {} is not a list\n", to_repr());
        std::exit(1);
    }

    auto to_repr() const -> std::string;

    friend object operator+(const object& lhs, const object& rhs);
    friend object operator-(const object& lhs, const object& rhs);
    friend object operator*(const object& lhs, const object& rhs);
    friend object operator/(const object& lhs, const object& rhs);
    friend object operator%(const object& lhs, const object& rhs);
    
    friend auto operator<=>(const object& lhs, const object& rhs) = default;

    friend bool operator||(const object& lhs, const object& rhs);
    friend bool operator&&(const object& lhs, const object& rhs);

    friend void swap(object& lhs, object& rhs);
};

auto is_int(std::string_view token) -> bool;
auto to_int(std::string_view token) -> int;

auto foramt_special_chars(const std::string& str) -> std::string;

}

template <> struct std::formatter<anzu::object> : std::formatter<std::string> {
    auto format(const anzu::object& obj, auto& ctx) {
        return std::formatter<std::string>::format(
            std::format("{}", obj.to_repr()), ctx
        );
    }
};