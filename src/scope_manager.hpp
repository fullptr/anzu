#pragma once
#include "object.hpp"
#include "utility/print.hpp"

#include <cstdint>
#include <optional>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <variant>

namespace anzu {
namespace exp {

struct variable
{
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
    scope_info                                d_info;
    std::shared_ptr<std::size_t>              d_next;
    bool                                      d_unsafe;
    std::unordered_map<std::string, variable> d_variables;

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
    {}

    auto declare(
        const std::string& name,
        const type_name& type,
        std::size_t location,
        std::size_t size,
        bool is_location_relative
    ) -> bool
    {
        const auto [_, success] = d_variables.try_emplace(
            name, type, location, size, is_location_relative
        );
        if (success) { *d_next += size; }
        return success;
    }

    auto find(const std::string& name) const -> std::optional<variable>
    {
        if (auto it = d_variables.find(name); it != d_variables.end()) {
            return it->second;
        }
        return std::nullopt;
    }

    auto is_global_scope() const { return std::holds_alternative<global_scope>(d_info); }
    auto is_function_scope() const -> bool { return std::holds_alternative<function_scope>(d_info); }
    auto is_block_scope() const -> bool { return std::holds_alternative<block_scope>(d_info); }
    auto is_loop_scope() const -> bool { return std::holds_alternative<loop_scope>(d_info); }

    auto get_location_counter() { return d_next; }
    auto is_unsafe() const -> bool { return d_unsafe; }
};

class scope_manager
{
    std::vector<scope> d_scopes;

public:
    scope_manager()
    {
        auto next = std::make_shared<std::size_t>(0);
        d_scopes.emplace_back(
            global_scope{ .next_var_location = next },
            next,
            false
        );
    }

    auto new_block_scope(bool unsafe) -> scope&
    {
        d_scopes.emplace_back(
            block_scope{},
            d_scopes.back().get_location_counter(),
            unsafe || d_scopes.back().is_unsafe()
        );
        return d_scopes.back();
    }

    auto new_function_scope(const type_name& return_type) -> scope&
    {
        auto next = std::make_shared<std::size_t>(0);
        d_scopes.emplace_back(
            function_scope{
                .return_type = return_type,
                .next_var_location = next
            },
            next,
            d_scopes.back().is_unsafe()
        );
        return d_scopes.back();
    }

    auto new_loop_scope() -> scope&
    {
        d_scopes.emplace_back(
            loop_scope{},
            d_scopes.back().get_location_counter(),
            d_scopes.back().is_unsafe()
        );
        return d_scopes.back();
    }

    auto pop_scope() -> scope
    {
        if (d_scopes.empty()) {
            print("Logic Error: Must also have at least one scope");
            std::exit(1);
        }
        auto s = std::move(d_scopes.back());
        d_scopes.pop_back();
        return s;
    }

    auto declare(
        const std::string& name, const type_name& type, std::size_t location, std::size_t size
    ) -> bool
    {
        const auto in_function = [&] {
            for (const auto& scope : d_scopes | std::views::reverse) {
                if (scope.is_function_scope()) return true;
            }
            return false;
        }();

        return d_scopes.back().declare(name, type, location, size, in_function);
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
};

}
}