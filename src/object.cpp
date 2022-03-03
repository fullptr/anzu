#include "object.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <algorithm>
#include <ranges>
#include <string_view>

namespace anzu {
namespace {

auto type_error(const anzu::object& lhs, const anzu::object& rhs, std::string_view op) -> void
{
    anzu::print("type error: cannot evaluate {} {} {}\n", lhs.to_repr(), op, rhs.to_repr());
    std::exit(1);
}

auto division_by_zero_error() -> void
{
    anzu::print("division by zero error\n");
    std::exit(1);
}

auto format_error(const std::string& str) -> void
{
    anzu::print("format error: could not format special chars in '{}'\n", str);
    std::exit(1);
}

auto type_error_conversion(std::string_view src_type, std::string_view dst_type) -> void
{
    anzu::print("type error: cannot convert {} to {}\n", src_type, dst_type);
    std::exit(1);
}

auto list_repr(const block_list& list) -> std::string
{
    const auto to_repr = [](const auto& o) { return o.to_repr(); };
    return std::format("[{}]", anzu::format_comma_separated(*list, to_repr));
}

}

auto format_special_chars(const std::string& str) -> std::string
{
    std::string ret;
    for (auto it = str.begin(); it != str.end(); ++it) {
        if (*it == '\\') {
            if (++it == str.end()) { format_error(str); }
            switch (*it) {
                break; case 'n': ret += '\n';
                break; case 't': ret += '\t';
                break; case 'r': ret += '\r';
                break; default: format_error(str);
            }
        }
        else {
            ret += *it;
        }
    }
    return ret;
}

auto object::to_int() const -> int
{
    return std::visit(overloaded {
        [](block_int v) { return v; },
        [](block_bool v) { return v ? 1 : 0; },
        [](const block_str& v) { return anzu::to_int(v); },
        [](const block_list& v) {
            type_error_conversion("list", "int");
            return 0;
        },
        [](object_null) {
            type_error_conversion("null", "int");
            return 0;
        }
    }, d_value);
}

auto object::to_bool() const -> bool
{
    return std::visit(overloaded {
        [](block_int v) { return v != 0; },
        [](block_bool v) { return v; },
        [](const block_str& v) { return v.size() > 0; },
        [](const block_list& v) { return v->size() > 0; },
        [](object_null) { return false; }
    }, d_value);
}

auto object::to_str() const -> std::string
{
    return std::visit(overloaded {
        [](block_int v) { return std::to_string(v); },
        [](block_bool v) { return std::string{v ? "true" : "false"}; },
        [](const block_str& v) { return v; },
        [](const block_list& v) { return list_repr(v); },
        [](object_null) { return std::string{"null"}; }
    }, d_value);
}

auto object::to_repr() const -> std::string
{
    return std::visit(overloaded {
        [](block_int val) { return std::to_string(val); },
        [](block_bool val) { return std::string{val ? "true" : "false"}; },
        [](const block_str& v) { return std::format("'{}'", v); },
        [](const block_list& v) { return list_repr(v); },
        [](object_null) { return std::string{"null"}; }
    }, d_value);
}

template <typename T>
concept addable = requires(T a, T b) { { a + b }; };

object operator+(const object& lhs, const object& rhs)
{
    return std::visit([]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (std::is_same_v<A, B> && addable<A>) {
            return object{a + b};
        } else {
            anzu::type_error(object{a}, object{b}, "+");
            return object{0};
        }
    }, lhs.d_value, rhs.d_value);
}

template <typename T>
concept subtractible = requires(T a, T b) { { a - b }; };

object operator-(const object& lhs, const object& rhs)
{
    return std::visit([&]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (std::is_same_v<A, B> && subtractible<A>) {
            return object{a - b};
        } else {
            anzu::type_error(object{a}, object{b}, "-");
            return object{0};
        }
    }, lhs.d_value, rhs.d_value);
}

template <typename T>
concept multipliable = requires(T a, T b) { { a * b }; };

object operator*(const object& lhs, const object& rhs)
{
    return std::visit([&]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (std::is_same_v<A, B> && multipliable<A>) {
            return object{a * b};
        } else {
            anzu::type_error(object{a}, object{b}, "*");
            return object{0};
        }
    }, lhs.d_value, rhs.d_value);
}

object operator/(const object& lhs, const object& rhs)
{
    return std::visit(overloaded {
        [](int a, int b) {
            if (b != 0) {
                return object{a / b};
            }
            anzu::division_by_zero_error();
            return object{0};
        },
        [](const auto& a, const auto& b) {
            anzu::type_error(object{a}, object{b}, "/");
            return object{0};
        }
    }, lhs.d_value, rhs.d_value);
}

object operator%(const object& lhs, const object& rhs)
{
    return std::visit(overloaded {
        [](int a, int b) {
            return object{a % b};
        },
        [](const auto& a, const auto& b) {
            anzu::type_error(object{a}, object{b}, "%");
            return object{0};
        }
    }, lhs.d_value, rhs.d_value);
}

bool operator||(const object& lhs, const object& rhs)
{
    return lhs.to_bool() || rhs.to_bool();
}

bool operator&&(const object& lhs, const object& rhs)
{
    return lhs.to_bool() && rhs.to_bool();
}

void swap(object& lhs, object& rhs)
{
    swap(lhs.d_value, rhs.d_value);
}

auto is_int(std::string_view token) -> bool
{
    auto it = token.begin();
    if (token.starts_with("-")) {
        std::advance(it, 1);
    }
    return std::all_of(it, token.end(), [](char c) { return std::isdigit(c); });
}

auto to_int(std::string_view token) -> int
{
    if (!is_int(token)) {
        anzu::print("type error: cannot convert '{}' to int\n", token);
        std::exit(1);
    }
    return std::stoi(std::string{token});
}

}