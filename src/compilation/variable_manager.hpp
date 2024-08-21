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
#include <filesystem>

namespace anzu {

class variable_manager;
class compiler;

struct variable
{
    std::filesystem::path module;
    std::string           name;
    type_name             type;
    std::size_t           location;
    std::size_t           size;
};

struct simple_scope
{
};

struct loop_scope
{
    std::vector<std::size_t> continues;
    std::vector<std::size_t> breaks;
};

using scope_info = std::variant<simple_scope, loop_scope>;

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
    bool d_local;

public:
    variable_manager(bool local = true) : d_local{local} {}
    auto declare(const std::filesystem::path& module, const std::string& name, const type_name& type, std::size_t size) -> bool;
    auto find(const std::filesystem::path& module, const std::string& name) const -> std::optional<variable>;
    auto scopes() const -> std::span<const scope> { return d_scopes; }

    auto in_loop() const -> bool;
    auto get_loop_info() -> loop_scope&;
    
    auto in_function() const -> bool;

    auto size() -> std::size_t;

    void new_scope();
    void new_loop_scope();
    void pop_scope(std::vector<std::byte>& code);

    // Functions to handle changes to control flow, ie- break, continue and return.
    // All of these can result in control flow not reaching the end of a scope
    auto handle_loop_exit(std::vector<std::byte>& code) -> void;
    auto handle_function_exit(std::vector<std::byte>& code) -> void;
};

}