#pragma once
#include "object.hpp"
#include "utility/common.hpp"

#include <cstdint>
#include <optional>
#include <vector>
#include <span>
#include <variant>

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

class scope
{
    scope_info            d_info;
    std::vector<variable> d_variables;
    std::size_t           d_start;
    std::size_t           d_next;

public:
    scope(const scope_info& info, std::size_t start_location)
        : d_info{info}
        , d_variables{}
        , d_start(start_location)
        , d_next(start_location)
    {}

    auto declare(
        const std::string& name,
        const type_name& type,
        std::size_t size,
        bool is_local
    ) -> bool
    {
        for (const auto& var : d_variables) {
            if (var.name == name) return false;
        }
        d_variables.emplace_back(name, type, d_next, size, is_local);
        d_next += size;
        return true;
    }

    auto scope_size() const -> std::size_t { return d_next - d_start; }

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
    auto as() -> ScopeType& { return std::get<ScopeType>(d_info); }

    auto next_location() { return d_next; }
};

class scope_manager
{
    std::vector<scope> d_scopes;

public:
    auto new_scope() -> void
    {
        d_scopes.emplace_back(
            simple_scope{},
            d_scopes.empty() ? 0 : d_scopes.back().next_location()
        );
    }

    auto new_function_scope(const type_name& return_type) -> void
    {
        d_scopes.emplace_back(
            function_scope{ .return_type = return_type },
            0
        );
    }

    auto new_loop_scope() -> void
    {
        d_scopes.emplace_back(
            loop_scope{},
            d_scopes.back().next_location()
        );
    }

    auto pop_scope() -> std::size_t
    {
        panic_if(d_scopes.empty(), "Tried to pop a scope, but there are none!");
        const auto size = d_scopes.back().scope_size();
        d_scopes.pop_back();
        return size;
    }

    auto declare(
        const std::string& name, const type_name& type, std::size_t size
    ) -> bool
    {
        const auto in_function = [&] {
            for (const auto& scope : d_scopes | std::views::reverse) {
                if (scope.is<function_scope>()) return true;
            }
            return false;
        }();

        return d_scopes.back().declare(name, type, size, in_function);
    }

    auto find(const std::string& name) const -> std::optional<variable>
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (const auto v = scope.find(name); v.has_value()) {
                return v;
            }
        }
        return std::nullopt;
    }

    auto in_loop() const -> bool
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (scope.is<loop_scope>()) return true;
        }
        return false;
    }

    auto in_function() const -> bool
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (scope.is<function_scope>()) return true;
        }
        return false;
    }

    auto get_loop_info() -> loop_scope&
    {
        for (auto& scope : d_scopes | std::views::reverse) {
            if (scope.is<loop_scope>()) {
                return scope.as<loop_scope>();
            }
        }
        panic("could not get loop info, not in a loop!");
    }

    auto get_function_info() -> function_scope&
    {
        for (auto& scope : d_scopes | std::views::reverse) {
            if (scope.is<function_scope>()) {
                return scope.as<function_scope>();
            }
        }
        panic("could not get function info, not in a function!");
    }

    inline auto size() -> std::size_t
    {
        return d_scopes.size();
    }
};

}