#pragma once
#include "object.hpp"
#include "program.hpp"

#include <vector>

namespace anzu {

struct runtime_context
{
    std::size_t prog_ptr = 0;
    std::size_t base_ptr = 0;
    std::vector<block> memory;
};

auto run_program(const program& prog) -> void;
auto run_program_debug(const program& prog) -> void;

}