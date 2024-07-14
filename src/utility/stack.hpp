#pragma once
#include "utility/common.hpp"
#include "utility/memory.hpp"

#include <utility>
#include <array>
#include <vector>
#include <cstdint>

namespace anzu {

struct value_stack
{
    std::vector<std::byte> d_data;
    std::size_t d_size = 0;

    value_stack() { d_data.resize(1024 * 1024 * 20); }

    template <typename T>
    auto pop() -> T
    {
        T ret{};
        d_size -= sizeof(T);
        std::memcpy(&ret, &d_data[d_size], sizeof(T));
        return ret;
    }

    template <typename T>
    auto push(const T& obj) -> void
    {
        if (d_size + sizeof(T) > d_data.size()) {
            std::print("Stack overflow");
            std::exit(27);
        }
        std::memcpy(&d_data[d_size], &obj, sizeof(T));
        d_size += sizeof(T);
    }

    auto size() const -> std::size_t
    {
        return d_size;
    }

    auto at(std::size_t index) -> std::byte&
    {
        return d_data[index];
    }

    auto at(std::size_t index) const -> const std::byte&
    {
        return d_data[index];
    }

    auto pop_n(std::size_t count) -> void
    {
        d_size -= count;
    }

    auto resize(std::size_t size) -> void
    {
        d_size = size;
    }
};


}