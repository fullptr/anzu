#pragma once
#include <span>
#include <utility>
#include <vector>
#include <map>

namespace anzu {

class memory_allocator
{
    std::vector<std::byte>             d_memory;
    std::map<std::size_t, std::size_t> d_pools;

public:
    auto allocate(std::size_t size) -> std::size_t;
    auto deallocate(std::size_t ptr, std::size_t size) -> void;

    auto operator[](std::size_t idx) -> std::byte& {
        return d_memory[idx];
    }

    auto memory() -> std::vector<std::byte>& { return d_memory; } // TODO: Encapsulate better
};

}