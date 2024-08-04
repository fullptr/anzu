#pragma once
#include <array>
#include <cstddef>
#include <vector>
#include <string>
#include <print>
#include <cstring>
#include <memory>

#include "bytecode.hpp"

namespace anzu {

struct call_frame
{
    std::byte* code = nullptr; // start of the current chunk of bytecode
    std::byte* ip = nullptr; // instruction pointer
    std::size_t base_ptr = 0;
};

class vm_stack
{
    std::unique_ptr<std::byte[]> d_data;
    std::size_t d_max_size;
    std::size_t d_current_size;

public:
    vm_stack(std::size_t size = 1024 * 1024 * 20);
    auto push(const std::byte* src, std::size_t count) -> void;
    auto pop_and_save(std::byte* dst, std::size_t count) -> void;
    auto save(std::byte* dst, std::size_t count) -> void;
    auto size() const -> std::size_t;
    auto at(std::size_t index) -> std::byte&;
    auto resize(std::size_t size) -> void;
    auto pop_n(std::size_t count) -> void;

    template <typename T>
    auto push(const T& obj) -> void
    {
        push(reinterpret_cast<const std::byte*>(&obj), sizeof(T));
    }

    template <typename T>
    auto pop() -> T
    {
        T ret{};
        d_current_size -= sizeof(T);
        std::memcpy(&ret, &d_data[d_current_size], sizeof(T));
        return ret;
    }

};

struct memory_arena
{
    std::array<std::byte, 1024 * 1024 * 64> data; // 64MB;
    std::size_t next = 0;
};

struct bytecode_context
{
    std::vector<bytecode_function> functions;
    std::string                    rom;

    std::vector<call_frame> frames = {};
    vm_stack stack                 = {};
    std::int64_t heap_size         = {};
};

auto run_program(const bytecode_program& prog) -> void;
auto run_program_debug(const bytecode_program& prog) -> void;

}