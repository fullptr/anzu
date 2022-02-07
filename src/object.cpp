#include "object.hpp"
#include "utility/print.hpp"

#include <algorithm>
#include <ranges>
#include <string_view>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

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
        [](int v) { return v; },
        [](bool v) { return v ? 1 : 0; },
        [](const std::string& v) { return anzu::to_int(v); },
        [](const object_list& v) {
            type_error_conversion("list", "int");
            return 0;
        },
        [](std::monostate) {
            type_error_conversion("null", "int");
            return 0;
        }
    }, d_value);
}

auto object::to_bool() const -> bool
{
    return std::visit(overloaded {
        [](int v) { return v != 0; },
        [](bool v) { return v; },
        [](const std::string& v) { return v.size() > 0; },
        [](const object_list& v) { return v->size() > 0; },
        [](std::monostate) { return false; }
    }, d_value);
}

auto object::to_str() const -> std::string
{
    return std::visit(overloaded {
        [](int v) { return std::to_string(v); },
        [](bool v) { return std::string{v ? "true" : "false"}; },
        [](const std::string& v) { return v; },
        [](const object_list& v) {
            switch (v->size()) {
                break; case 0:
                    return std::string{"[]"};
                break; case 1:
                    return std::format("[{}]", v->at(0));
                break; default:
                    std::string ret = std::format("[{}", v->at(0));
                    for (const auto& obj : *v | std::views::drop(1)) {
                        ret += std::format(", {}", obj.to_repr());
                    }
                    ret += "]";
                    return ret;
            }
        },
        [](std::monostate) { return std::string{"null"}; }
    }, d_value);
}

auto object::to_repr() const -> std::string
{
    return std::visit(overloaded {
        [](int val) { return std::to_string(val); },
        [](bool val) { return std::string{val ? "true" : "false"}; },
        [](const std::string& v) { return std::format("'{}'", v); },
        [](const object_list& v) {
            switch (v->size()) {
                break; case 0:
                    return std::string{"[]"};
                break; case 1:
                    return std::format("[{}]", v->at(0));
                break; default:
                    std::string ret = std::format("[{}", v->at(0));
                    for (const auto& obj : *v | std::views::drop(1)) {
                        ret += std::format(", {}", obj.to_repr());
                    }
                    ret += "]";
                    return ret;
            }
        },
        [](std::monostate) { return std::string{"null"}; }
    }, d_value);
}

template <typename T>
concept addable = requires(T a, T b) { { a + b }; };

object operator+(const object& lhs, const object& rhs)
{
    return std::visit([]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (std::is_same_v<A, B> && addable<A>) {
            return a + b;
        } else {
            anzu::type_error(a, b, "+");
            return 0;
        }
    }, lhs.d_value, rhs.d_value);
}

template <typename T>
concept subtractible = requires(T a, T b) { { a - b }; };

object operator-(const object& lhs, const object& rhs)
{
    return std::visit([&]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (std::is_same_v<A, B> && subtractible<A>) {
            return a - b;
        } else {
            anzu::type_error(a, b, "-");
            return 0;
        }
    }, lhs.d_value, rhs.d_value);
}

template <typename T>
concept multipliable = requires(T a, T b) { { a * b }; };

object operator*(const object& lhs, const object& rhs)
{
    return std::visit([&]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (std::is_same_v<A, B> && multipliable<A>) {
            return a * b;
        } else {
            anzu::type_error(a, b, "*");
            return 0;
        }
    }, lhs.d_value, rhs.d_value);
}

object operator/(const object& lhs, const object& rhs)
{
    return std::visit([&]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (std::is_same_v<A, int> && std::is_same_v<B, int>) {
            if (b != 0) {
                return a / b;
            }
            anzu::division_by_zero_error();
            return 0;
        } else {
            anzu::type_error(a, b, "/");
            return 0;
        }
    }, lhs.d_value, rhs.d_value);
}

template <typename A, typename B>
concept moddable = requires(A a, B b) { { a % b }; };

object operator%(const object& lhs, const object& rhs)
{
    return std::visit([&]<typename A, typename B>(const A& a, const B& b) -> anzu::object {
        if constexpr (moddable<A, B>) {
            return a % b;
        } else {
            anzu::type_error(a, b, "%");
            return 0;
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