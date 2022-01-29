#pragma once
#include "object.hpp"
#include "op_codes.hpp"

#include <stack>
#include <unordered_map>
#include <string>
#include <variant>
#include <ranges>

namespace anzu {

class memory
{
    std::unordered_map<std::string, anzu::object> d_values;

public:
    auto insert(const std::string& name, const anzu::object& value) -> anzu::object&;
    auto get(const std::string& name) -> anzu::object&;
    auto print() const -> void;
};

struct frame
{
    anzu::memory  memory;
    std::intptr_t ptr = 0;
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
};

auto run_program(const std::vector<anzu::op>& program) -> void;
auto run_program_debug(const std::vector<anzu::op>& program) -> void;

}