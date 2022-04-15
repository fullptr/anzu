#pragma once
#include <array>
#include <vector>
#include <utility>
#include <cstdint>
#include <cstring>

namespace anzu {

inline static constexpr auto SIZE64 = sizeof(std::uint64_t);

template <std::size_t N>
auto pop_n(std::vector<std::byte>& vec) -> void
{
    vec.resize(vec.size() - N);
}

auto push_u64(std::vector<std::byte>& mem, std::uint64_t value) -> void
{
    for (const auto& b : std::bit_cast<std::array<std::byte, SIZE64>>(value)) {
        mem.push_back(b);
    }
}

auto pop_u64(std::vector<std::byte>& mem) -> std::uint64_t
{
    auto ret = std::uint64_t{};
    std::memcpy(&ret, mem.data() + mem.size() - SIZE64, SIZE64);
    pop_n<SIZE64>(mem);
    return ret;
}

auto write_u64(std::vector<std::byte>& mem, std::size_t ptr, std::uint64_t value) -> void
{
    std::memcpy(&mem[ptr], &value, SIZE64);
}

auto read_u64(std::vector<std::byte>& mem, std::size_t ptr) -> std::uint64_t
{
    auto ret = std::uint64_t{};
    std::memcpy(&ret, &mem[ptr], SIZE64);
    return ret;
}

}