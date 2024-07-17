#pragma once
#include "utility/common.hpp"
#include "utility/memory.hpp"

#include <utility>
#include <array>
#include <vector>
#include <cstdint>

namespace anzu {

class vm_stack
{
    std::unique_ptr<std::byte[]> d_data;
    std::size_t d_max_size;
    std::size_t d_current_size;

public:
    vm_stack(std::size_t size = 1024 * 1024 * 20)
        : d_data{std::make_unique<std::byte[]>(size)}
        , d_max_size{size}
        , d_current_size{0}
    {}

    auto push(const std::byte* src, std::size_t count) -> void
    {
        if (d_current_size + count > d_max_size) {
            std::print("Stack overflow\n");
            std::exit(27);
        }
        std::memcpy(&d_data[d_current_size], src, count);
        d_current_size += count;
    }

    template <typename T>
    auto push(const T& obj) -> void
    {
        push(reinterpret_cast<const std::byte*>(&obj), sizeof(T));
    }

    auto pop_and_save(std::byte* dst, std::size_t count) -> void
    {
        if (d_current_size < count) {
            std::print("Stack underflow\n");
            std::exit(28);
        }
        d_current_size -= count;
        std::memcpy(dst, &d_data[d_current_size], count);
    }

    template <typename T>
    auto pop() -> T
    {
        T ret{};
        d_current_size -= sizeof(T);
        std::memcpy(&ret, &d_data[d_current_size], sizeof(T));
        return ret;
    }

    inline auto size() const -> std::size_t { return d_current_size; }
    inline auto at(std::size_t index) -> std::byte& { return d_data[index]; }
    inline auto at(std::size_t index) const -> const std::byte& { return d_data[index]; }
    inline auto pop_n(std::size_t count) -> void { d_current_size -= count; }
    inline auto resize(std::size_t size) -> void { d_current_size = size; }
};


}