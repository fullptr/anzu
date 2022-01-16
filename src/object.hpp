#pragma once
#include <string>
#include <variant>
#include <format>

namespace anzu {

class object
{
    using value_type = std::variant<
        int,
        bool,
        std::string
    >;

    value_type d_value;

public:
    template <typename Obj>
    object(const Obj& obj) : d_value{obj} {}
    
    object() : d_value{0} {}

    auto is_int() const -> bool;
    auto is_bool() const -> bool;
    auto is_str() const -> bool;

    auto to_int() const -> int;
    auto to_bool() const -> bool;
    auto to_str() const -> std::string;

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
            std::format("{}", obj.to_str()), ctx
        );
    }
};