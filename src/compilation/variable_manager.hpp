#pragma once
#include "object.hpp"
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
class compiler;

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

class variable_manager
{
    std::vector<scope> d_scopes;

public:
    auto declare(const std::string& name, const type_name& type, std::size_t size) -> bool;
    auto find(const std::string& name) const -> std::optional<variable>;
    auto scopes() const -> std::span<const scope> { return d_scopes; }

    auto in_loop() const -> bool;
    auto get_loop_info() -> loop_scope&;
    
    auto in_function() const -> bool;
    auto get_function_info() -> function_scope&;

    auto size() -> std::size_t;

    void new_scope();
    void new_function_scope();
    void new_loop_scope();
    void pop_scope(std::vector<std::byte>& code);

    // Functions to handle changes to control flow, ie- break, continue and return.
    // All of these can result in control flow not reaching the end of a scope
    auto handle_loop_exit(std::vector<std::byte>& code) -> void;
    auto handle_function_exit(std::vector<std::byte>& code) -> void;
};

}