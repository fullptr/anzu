#pragma once
#include "object.hpp"
#include "program.hpp"

#include <vector>

namespace anzu {

struct frame
{
    std::size_t program_ptr = 0;
    std::size_t return_size = 1;
};

struct runtime_context
{
    std::size_t base_ptr = 0;
    std::vector<frame> frames;
    std::vector<block> memory;
};

auto run_program(const program& prog) -> void;
auto run_program_debug(const program& prog) -> void;

}