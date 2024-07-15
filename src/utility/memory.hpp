#pragma once
#include <array>
#include <vector>
#include <utility>
#include <cstdint>
#include <cstring>

namespace anzu {

template <typename T>
auto push_value_single(std::vector<std::byte>& mem, const T& value) -> void
{
    const auto old_size = mem.size();
    mem.resize(mem.size() + sizeof(T));
    std::memcpy(&mem[old_size], &value, sizeof(T));
}

template <typename... Ts>
auto push_value(std::vector<std::byte>& mem, Ts&&... values) -> std::size_t
{
    const auto size = mem.size();
    (push_value_single(mem, std::forward<Ts>(values)), ...);
    return size;
}

template <typename T>
auto write_value(std::vector<std::byte>& mem, std::size_t ptr, const T& value) -> void
{
    std::memcpy(&mem[ptr], &value, sizeof(T));
}

}