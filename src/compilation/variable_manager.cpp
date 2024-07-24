#include "variable_manager.hpp"

namespace anzu {
namespace {

auto delete_arena(std::vector<std::byte>& program, const variable& arena)
{
     // Push the arena onto the stack
    const auto op = arena.is_local ? op::push_ptr_local : op::push_ptr_global;
    push_value(program, op, arena.location, op::load, sizeof(std::byte*));

    // and delete it
    push_value(program, op::arena_delete);
}

auto delete_arenas_in_scope(std::vector<std::byte>& program, const scope& scope)
{
    for (const auto& variable : scope.d_variables | std::views::reverse) {
        if (variable.type == arena_type()) {
            delete_arena(program, variable);
        }
    }
}

}

scope::scope(const scope_info& info, std::size_t start_location)
    : d_info{info}
    , d_variables{}
    , d_start(start_location)
    , d_next(start_location)
{}

auto scope::scope_size() const -> std::size_t { return d_next - d_start; }

auto scope::find(const std::string& name) const -> std::optional<variable>
{
    for (const auto& var : d_variables) {
        if (var.name == name) return var;
    }
    return std::nullopt;
}

auto scope::next_location() -> std::size_t { return d_next; }

auto variable_manager::push_scope() -> void
{
    d_scopes.emplace_back(
        simple_scope{},
        d_scopes.empty() ? 0 : d_scopes.back().next_location()
    );
}

auto variable_manager::new_scope() -> scope_guard
{
    push_scope();
    return scope_guard{*this};
}

auto variable_manager::new_function_scope(const type_name& return_type) -> scope_guard
{
    push_function_scope(return_type);
    return scope_guard{*this};
}

auto variable_manager::new_loop_scope() -> scope_guard
{
    push_loop_scope();
    return scope_guard{*this};
}

auto variable_manager::push_function_scope(const type_name& return_type) -> void
{
    d_scopes.emplace_back(function_scope{return_type}, 0);
}

auto variable_manager::push_loop_scope() -> void
{
    d_scopes.emplace_back(loop_scope{}, d_scopes.back().next_location());
}

auto variable_manager::pop_scope() -> std::size_t
{
    panic_if(d_scopes.empty(), "Tried to pop a scope, but there are none!");
    const auto size = d_scopes.back().scope_size();
    d_scopes.pop_back();
    return size;
}

auto variable_manager::declare(
    const std::string& name, const type_name& type, std::size_t size
) -> bool
{
    const auto in_function = [&] {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (scope.is<function_scope>()) return true;
        }
        return false;
    }();

    auto& scope = d_scopes.back();
    for (const auto& var : scope.d_variables) {
        if (var.name == name) return false;
    }
    scope.d_variables.emplace_back(std::string{name}, type, scope.d_next, size, in_function);
    scope.d_next += size;
    return true;
}

auto variable_manager::find(const std::string& name) const -> std::optional<variable>
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        if (const auto v = scope.find(name); v.has_value()) {
            return v;
        }
    }
    return std::nullopt;
}

auto variable_manager::in_loop() const -> bool
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        if (scope.is<loop_scope>()) return true;
    }
    return false;
}

auto variable_manager::in_function() const -> bool
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        if (scope.is<function_scope>()) return true;
    }
    return false;
}

auto variable_manager::get_loop_info() -> loop_scope&
{
    for (auto& scope : d_scopes | std::views::reverse) {
        if (scope.is<loop_scope>()) {
            return scope.as<loop_scope>();
        }
    }
    panic("could not get loop info, not in a loop!");
}

auto variable_manager::get_function_info() -> function_scope&
{
    for (auto& scope : d_scopes | std::views::reverse) {
        if (scope.is<function_scope>()) {
            return scope.as<function_scope>();
        }
    }
    panic("could not get function info, not in a function!");
}

auto variable_manager::size() -> std::size_t
{
    return d_scopes.size();
}

auto variable_manager::handle_loop_exit() -> void
{
    auto pop_size = std::size_t{0};
    for (const auto& scope : d_scopes | std::views::reverse) {
        delete_arenas_in_scope(*d_program, scope);
        pop_size += scope.d_next - scope.d_start;
        if (scope.is<loop_scope>()) break;
    }
    push_value(*d_program, op::pop, pop_size);
}

// This should NOT pop data from the stack like handle_loop_exit since the
// op::ret op code does this for us.
auto variable_manager::handle_function_exit() -> void
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        delete_arenas_in_scope(*d_program, scope);
        if (scope.is<function_scope>()) break;
    }
}

scope_guard::scope_guard(variable_manager& manager)
    : d_manager{&manager}
{}

scope_guard::~scope_guard() {
    // Delete any arenas in the current scope
    delete_arenas_in_scope(*d_manager->d_program, d_manager->d_scopes.back());

    // Then pop all local variables
    const auto scope_size = d_manager->pop_scope();
    if (scope_size > 0) {
        push_value(*d_manager->d_program, op::pop, scope_size);
    }    
}

}