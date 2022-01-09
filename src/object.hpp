#pragma once
#include <string>
#include <variant>
#include <fmt/format.h>

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

auto is_int(const std::string& token) -> bool;
auto to_int(const std::string& token) -> int;

}

template <> struct fmt::formatter<anzu::object> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.end(); }
    auto format(const anzu::object& obj, auto& ctx) {
        return format_to(ctx.out(), obj.to_str());
    }
};