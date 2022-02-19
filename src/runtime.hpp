#pragma once
#include "object.hpp"
#include "program.hpp"

#include <stack>
#include <string>
#include <variant>
#include <ranges>

namespace anzu {

struct frame
{
    std::intptr_t ptr = 0;
    std::intptr_t base_ptr = 0;
};

class runtime_context
{
    std::vector<frame>        d_frames;
    std::vector<anzu::object> d_values;

public:
    auto push_frame() -> frame&;
    auto pop_frame() -> void;
    auto peek_frame(std::size_t index = 0) -> frame&;

    auto push_value(const object& val) -> object&;
    auto pop_value() -> object;
    auto peek_value(std::size_t index = 0) -> object&;
    auto size() const -> std::size_t;

    std::vector<anzu::object> values;
};

auto run_program(const anzu::program& program) -> void;
auto run_program_debug(const anzu::program& program) -> void;

}