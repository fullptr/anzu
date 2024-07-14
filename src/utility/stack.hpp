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
    std::vector<std::byte> data;

    template <typename T>
    auto pop() -> T
    {
        return anzu::pop_value<T>(data);
    }

    template <typename T>
    auto push(const T& obj) -> void
    {
        anzu::push_value(data, obj);
    }

    auto size() const -> std::size_t
    {
        return data.size();
    }

    auto at(std::size_t index) -> std::byte&
    {
        return data[index];
    }

    auto at(std::size_t index) const -> const std::byte&
    {
        return data[index];
    }

    auto pop_n(std::size_t count) -> void
    {
        data.resize(data.size() - count);
    }

    auto resize(std::size_t size) -> void
    {
        data.resize(size);
    }
};


}