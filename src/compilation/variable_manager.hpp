#pragma once
#include "object.hpp"
#include "bytecode.hpp"
#include "utility/memory.hpp"
#include "utility/common.hpp"

#include <cstdint>
#include <optional>
#include <vector>
#include <span>
#include <variant>
#include <ranges>

namespace anzu {

class variable_manager;

struct variable
{
    std::string name;
    type_name   type;
    std::size_t location;
    std::size_t size;
    bool        is_local;
};

struct simple_scope
{
};

struct function_scope
{
    type_name return_type;
};

struct loop_scope
{
    std::vector<std::size_t> continues;
    std::vector<std::size_t> breaks;
};

using scope_info = std::variant<simple_scope, function_scope, loop_scope>;

struct scope
{
    scope_info            info;
    std::size_t           start;
    std::size_t           next      = start;
    std::vector<variable> variables = {};
};

class scope_guard
{
    variable_manager* d_manager;
    scope_guard(const scope_guard&) = delete;
    scope_guard& operator=(const scope_guard&) = delete;

public:
    scope_guard(variable_manager& manager);
    ~scope_guard();
};

class variable_manager
{
    std::vector<std::byte>* d_program;
    std::vector<scope> d_scopes;
    friend scope_guard;

public:
    auto set_program(std::vector<std::byte>* program) { d_program = program; }

    auto declare(const std::string& name, const type_name& type, std::size_t size) -> bool;
    auto find(const std::string& name) const -> std::optional<variable>;
    auto scopes() const -> std::span<const scope> { return d_scopes; }

    auto in_loop() const -> bool;
    auto get_loop_info() -> loop_scope&;
    
    auto in_function() const -> bool;
    auto get_function_info() -> function_scope&;

    auto size() -> std::size_t;

    auto new_scope() -> scope_guard;
    auto new_function_scope(const type_name& return_type) -> scope_guard;
    auto new_loop_scope() -> scope_guard;

    // Functions to handle changes to control flow, ie- break, continue and return.
    // All of these can result in control flow not reaching the end of a scope
    auto handle_loop_exit() -> void;
    auto handle_function_exit() -> void;
};

}