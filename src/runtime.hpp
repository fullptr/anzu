#pragma once
#include "object.hpp"
#include "program.hpp"

#include <vector>

namespace anzu {

struct frame
{
    std::intptr_t program_ptr = 0;
    std::intptr_t base_ptr = 0;
};

struct runtime_context
{
    std::vector<frame>  frames;
    std::vector<block> memory;
};

auto run_program(const program& prog) -> void;
auto run_program_debug(const program& prog) -> void;

}