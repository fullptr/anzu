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
    std::vector<frame>        frames;
    std::vector<anzu::object> stack;
    std::vector<anzu::object> memory;
};

auto run_program(const anzu::program& program) -> void;
auto run_program_debug(const anzu::program& program) -> void;

}