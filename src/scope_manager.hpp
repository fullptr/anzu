#pragma once
#include "object.hpp"
#include "utility/common.hpp"

#include <cstdint>
#include <optional>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <span>
#include <variant>

namespace anzu {

struct variable
{
    std::string name;
    type_name   type;
    std::size_t location;
    std::size_t size;
    bool        is_location_relative;
};

struct global_scope
{
    std::shared_ptr<std::size_t> next_var_location;
};

struct function_scope
{
    type_name                    return_type;
    std::shared_ptr<std::size_t> next_var_location;
};

struct block_scope
{
};

struct loop_scope
{
    std::unordered_set<std::size_t> continues;
    std::unordered_set<std::size_t> breaks;
};

using scope_info = std::variant<
    global_scope,
    function_scope,
    block_scope,
    loop_scope
>;

class scope
{
    scope_info                   d_info;
    std::shared_ptr<std::size_t> d_next;
    bool                         d_unsafe;
    std::vector<variable>        d_variables;
    std::size_t                  d_scope_size;

public:
    scope(
        const scope_info& info,
        const std::shared_ptr<std::size_t>& next_var_location,
        bool unsafe
    )
        : d_info{info}
        , d_next(next_var_location)
        , d_unsafe{unsafe}
        , d_variables{}
        , d_scope_size{0}
    {}

    ~scope()
    {   
        for (const auto var : d_variables) {
            *d_next -= var.size;
        }
    }

    auto declare(
        const std::string& name,
        const type_name& type,
        std::size_t size,
        bool is_location_relative
    ) -> bool
    {
        for (const auto& var : d_variables) {
            if (var.name == name) return false;
        }
        d_variables.emplace_back(name, type, *d_next, size, is_location_relative);
        *d_next += size;
        d_scope_size += size;
        return true;
    }

    auto scope_size() const -> std::size_t { return d_scope_size; }

    auto find(const std::string& name) const -> std::optional<variable>
    {
        for (const auto& var : d_variables) {
            if (var.name == name) return var;
        }
        return std::nullopt;
    }

    template <typename ScopeType>
    auto is() const -> bool { return std::holds_alternative<ScopeType>(d_info); }

    template <typename ScopeType>
    auto as() const -> const ScopeType& { return std::get<ScopeType>(d_info); }

    template <typename ScopeType>
    auto as() -> ScopeType& { return std::get<ScopeType>(d_info); }

    auto get_location_counter() { return d_next; }
    auto is_unsafe() const -> bool { return d_unsafe; }

    auto variables() const -> std::span<const variable> { return d_variables; }
};

class scope_manager
{
    std::vector<std::shared_ptr<scope>> d_scopes;

public:

    auto current() -> std::shared_ptr<scope>
    {
        return d_scopes.back();
    }

    auto new_global_scope() -> void
    {
        panic_if(!d_scopes.empty(), "Can only have a global scope at the top level");
        auto next = std::make_shared<std::size_t>(0);
        d_scopes.emplace_back(std::make_shared<scope>(
            global_scope{ .next_var_location = next },
            next,
            false
        ));
    }

    auto new_block_scope(bool unsafe) -> void
    {
        panic_if(d_scopes.empty(), "Cannot add a block scope before a global scope");
        d_scopes.emplace_back(std::make_shared<scope>(
            block_scope{},
            current()->get_location_counter(),
            unsafe || current()->is_unsafe()
        ));
    }

    auto new_function_scope(const type_name& return_type) -> void
    {
        panic_if(d_scopes.empty(), "Cannot add a function scope before a global scope");
        auto next = std::make_shared<std::size_t>(0);
        d_scopes.emplace_back(std::make_shared<scope>(
            function_scope{
                .return_type = return_type,
                .next_var_location = next
            },
            next,
            current()->is_unsafe()
        ));
    }

    auto new_loop_scope() -> void
    {
        panic_if(d_scopes.empty(), "Cannot add a loop scope before a global scope");
        d_scopes.emplace_back(std::make_shared<scope>(
            loop_scope{},
            current()->get_location_counter(),
            current()->is_unsafe()
        ));
    }

    auto pop_scope() -> void
    {
        panic_if(d_scopes.empty(), "Tried to pop a scope, but there are none!");
        d_scopes.pop_back();
    }

    auto declare(
        const std::string& name, const type_name& type, std::size_t size
    ) -> bool
    {
        const auto in_function = [&] {
            for (const auto& scope : d_scopes | std::views::reverse) {
                if (scope->is<function_scope>()) return true;
            }
            return false;
        }();

        return d_scopes.back()->declare(name, type, size, in_function);
    }

    auto find(const std::string& name) const -> std::optional<variable>
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (const auto v = scope->find(name); v.has_value()) {
                return v;
            }
        }
        return std::nullopt;
    }

    auto in_loop() const -> bool
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (scope->is<loop_scope>()) return true;
        }
        return false;
    }

    auto in_function() const -> bool
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (scope->is<function_scope>()) return true;
        }
        return false;
    }

    // We propagate the unsafeness value, so only need to check the top scope
    auto in_unsafe() const -> bool
    {
        return d_scopes.back()->is_unsafe();
    }

    auto get_loop_info() -> loop_scope&
    {
        for (auto& scope : d_scopes | std::views::reverse) {
            if (scope->is<loop_scope>()) {
                return scope->as<loop_scope>();
            }
        }
        panic("could not get loop info, not in a loop!");
    }

    auto get_function_info() -> function_scope&
    {
        for (auto& scope : d_scopes | std::views::reverse) {
            if (scope->is<function_scope>()) {
                return scope->as<function_scope>();
            }
        }
        panic("could not get function info, not in a function!");
    }

    auto all() -> std::span<const std::shared_ptr<scope>>
    {
        return d_scopes;
    }
};

}