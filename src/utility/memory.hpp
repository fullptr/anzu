#pragma once
#include <array>
#include <vector>
#include <utility>
#include <cstdint>
#include <cstring>

namespace anzu {

inline auto pop_n(std::vector<std::byte>& vec, std::size_t count) -> void
{
    vec.resize(vec.size() - count);
}

template <typename T>
auto push_value(std::vector<std::byte>& mem, const T& value) -> void
{
    const auto bytes = std::bit_cast<std::array<std::byte, sizeof(T)>>(value);
    for (const auto& b : bytes) {
        mem.push_back(b);
    }
}

template <typename T>
auto pop_value(std::vector<std::byte>& mem) -> T
{
    auto ret = T{};
    std::memcpy(&ret, &mem[mem.size() - sizeof(T)], sizeof(T));
    mem.resize(mem.size() - sizeof(T));
    return ret;
}

template <typename T>
auto read_top(std::vector<std::byte>& mem) -> T
{
    auto ret = T{};
    std::memcpy(&ret, &mem[mem.size() - sizeof(T)], sizeof(T));
    return ret;
}

template <typename T>
auto write_value(std::vector<std::byte>& mem, std::size_t ptr, const T& value) -> void
{
    std::memcpy(&mem[ptr], &value, sizeof(T));
}

template <typename T>
auto read_value(std::vector<std::byte>& mem, std::size_t ptr) -> T
{
    auto ret = T{};
    std::memcpy(&ret, &mem[ptr], sizeof(T));
    return ret;
}

}