#pragma once
#include <iostream>
#include <format>
#include <string_view>
#include <vector>
#include <ranges>

namespace anzu {

template <typename... Args>
void print(std::string_view fmt, Args&&... args)
{
    std::cout << std::format(fmt, std::forward<Args>(args)...);
}

template <typename T, typename Func>
auto format_comma_separated(const std::vector<T>& values, Func&& formatter) -> std::string
{
    std::string ret;
    if (!values.empty()) {
        ret += formatter(values.front());
        for (const auto& value : values | std::views::drop(1)) {
            ret += std::format(", {}", formatter(value));
        }
    }
    return ret;
}

template <typename T, typename Func>
auto print_comma_separated(const std::vector<T>& values, Func&& formatter) -> void
{
    std::cout << format_comma_separated(values, std::forward<Func>(formatter));
}

}