#pragma once
#include <iostream>
#include <format>
#include <string_view>
#include <vector>
#include <ranges>

namespace anzu {

template <typename... Args>
void print(std::format_string<Args...> fmt, Args&&... args)
{
    std::cout << std::format(fmt, std::forward<Args>(args)...);
}

template <typename T, typename Transform>
auto format_comma_separated(const std::vector<T>& values, Transform&& transform) -> std::string
{
    std::string ret;
    if (!values.empty()) {
        ret += std::format("{}", transform(values.front()));
        for (const auto& value : values | std::views::drop(1)) {
            ret += std::format(", {}", transform(value));
        }
    }
    return ret;
}

template <typename T>
auto format_comma_separated(const std::vector<T>& values) -> std::string
{
    return format_comma_separated(values, [](auto&& x) { return std::format("{}", x); });
}

template <typename T, typename Transform>
auto print_comma_separated(const std::vector<T>& values, Transform&& transform) -> void
{
    std::cout << format_comma_separated(values, std::forward<Transform>(transform));
}

}