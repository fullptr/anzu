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
    scope_info            d_info;
    std::vector<variable> d_variables;
    std::size_t           d_start;
    std::size_t           d_next;

public:
    scope(const scope_info& info, std::size_t start_location);
    auto declare(std::string_view name, const type_name& type, std::size_t size, bool is_local) -> bool;
    auto scope_size() const -> std::size_t;
    auto find(const std::string& name) const -> std::optional<variable>;
    auto next_location() -> std::size_t;

    template <typename ScopeType>
    auto is() const -> bool { return std::holds_alternative<ScopeType>(d_info); }

    template <typename ScopeType>
    auto as() -> ScopeType& { return std::get<ScopeType>(d_info); }
};

class scope_guard;

class variable_manager
{
    std::vector<std::byte>* d_program;
    std::vector<scope> d_scopes;

    auto push_scope() -> void;
    auto push_function_scope(const type_name& return_type) -> void;
    auto push_loop_scope() -> void;
    auto pop_scope() -> std::size_t;

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

    friend scope_guard;
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

}