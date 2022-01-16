#pragma once
#include <iostream>
#include <format>
#include <string_view>

namespace anzu {

template <typename... Args>
void print(std::string_view fmt, Args&&... args)
{
    std::cout << std::format(fmt, std::forward<Args>(args)...);
}

}