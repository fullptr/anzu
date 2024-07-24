#include "variable_manager.hpp"

namespace anzu {
namespace {

auto delete_arenas_in_scope(std::vector<std::byte>& program, const scope& scope)
{
    for (const auto& var : scope.variables | std::views::reverse) {
        if (var.type.is_arena()) {
            const auto op = var.is_local ? op::push_ptr_local : op::push_ptr_global;
            push_value(program, op, var.location, op::load, sizeof(std::byte*), op::arena_delete);
        }
    }
}

}

auto variable_manager::new_scope() -> scope_guard
{
    d_scopes.emplace_back(
        simple_scope{},
        d_scopes.empty() ? 0 : d_scopes.back().next
    );
    return scope_guard{*this};
}

auto variable_manager::new_function_scope(const type_name& return_type) -> scope_guard
{
    d_scopes.emplace_back(function_scope{return_type}, 0);
    return scope_guard{*this};
}

auto variable_manager::new_loop_scope() -> scope_guard
{
    d_scopes.emplace_back(loop_scope{}, d_scopes.back().next);
    return scope_guard{*this};
}

auto variable_manager::declare(
    const std::string& name, const type_name& type, std::size_t size
) -> bool
{
    auto& scope = d_scopes.back();
    for (const auto& var : scope.variables) {
        if (var.name == name) return false;
    }
    scope.variables.emplace_back(name, type, scope.next, size, in_function());
    scope.next += size;
    return true;
}

auto variable_manager::find(const std::string& name) const -> std::optional<variable>
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        for (const auto& var : scope.variables) { // no need to reverse here, var names are unique per scope
            if (var.name == name) return var;
        }
    }
    return std::nullopt;
}

auto variable_manager::in_loop() const -> bool
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        if (std::holds_alternative<loop_scope>(scope.info)) return true;
    }
    return false;
}

auto variable_manager::in_function() const -> bool
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        if (std::holds_alternative<function_scope>(scope.info)) return true;
    }
    return false;
}

auto variable_manager::get_loop_info() -> loop_scope&
{
    for (auto& scope : d_scopes | std::views::reverse) {
        if (auto loop = std::get_if<loop_scope>(&scope.info)) {
            return *loop;
        }
    }
    panic("could not get loop info, not in a loop!");
}

auto variable_manager::get_function_info() -> function_scope&
{
    for (auto& scope : d_scopes | std::views::reverse) {
        if (auto func = std::get_if<function_scope>(&scope.info)) {
            return *func;
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
        pop_size += scope.next - scope.start;
        if (std::holds_alternative<loop_scope>(scope.info)) break;
    }
    push_value(*d_program, op::pop, pop_size);
}

// This should NOT pop data from the stack like handle_loop_exit since the
// op::ret op code does this for us.
auto variable_manager::handle_function_exit() -> void
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        delete_arenas_in_scope(*d_program, scope);
        if (std::holds_alternative<function_scope>(scope.info)) break;
    }
}

scope_guard::scope_guard(variable_manager& manager)
    : d_manager{&manager}
{}

scope_guard::~scope_guard() {
    // Delete any arenas in the current scope
    delete_arenas_in_scope(*d_manager->d_program, d_manager->d_scopes.back());

    // Then pop all local variables
    panic_if(d_manager->d_scopes.empty(), "Tried to pop a scope, but there are none!");
    const auto& scope = d_manager->d_scopes.back();
    const auto size = scope.next - scope.start;
    d_manager->d_scopes.pop_back();

    if (size > 0) {
        push_value(*d_manager->d_program, op::pop, size);
    }    
}

}