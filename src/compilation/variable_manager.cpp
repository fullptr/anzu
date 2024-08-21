#include "variable_manager.hpp"
#include "compiler.hpp"

namespace anzu {
namespace {

auto delete_arenas_in_scope(std::vector<std::byte>& program, const scope& scope, bool is_local)
{
    for (const auto& var : scope.variables | std::views::reverse) {
        if (var.type.is_arena()) {
            const auto op = is_local ? op::push_ptr_local : op::push_ptr_global;
            push_value(program, op, var.location, op::load, sizeof(std::byte*), op::arena_delete);
        }
    }
}

}

void variable_manager::new_scope()
{
    d_scopes.emplace_back(
        simple_scope{},
        d_scopes.empty() ? 0 : d_scopes.back().next
    );
}

void variable_manager::new_loop_scope()
{
    d_scopes.emplace_back(loop_scope{}, d_scopes.back().next);
}

auto variable_manager::declare(
    const std::filesystem::path& module, const std::string& name, const type_name& type, std::size_t size
) -> bool
{
    auto& scope = d_scopes.back();
    for (const auto& var : scope.variables) {
        if (var.name == name && var.module == module) return false;
    }
    scope.variables.emplace_back(module, name, type, scope.next, size);
    scope.next += size;
    return true;
}

auto variable_manager::find(const std::filesystem::path& module, const std::string& name) const -> std::optional<variable>
{
    for (const auto& scope : d_scopes | std::views::reverse) {
        for (const auto& var : scope.variables) { // no need to reverse here, var names are unique per scope
            if (var.name == name && var.module == module) return var;
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
    return d_local;
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

auto variable_manager::size() -> std::size_t
{
    return d_scopes.size();
}

auto variable_manager::handle_loop_exit(std::vector<std::byte>& code) -> void
{
    auto pop_size = std::size_t{0};
    for (const auto& scope : d_scopes | std::views::reverse) {
        delete_arenas_in_scope(code, scope, d_local);
        pop_size += scope.next - scope.start;
        if (std::holds_alternative<loop_scope>(scope.info)) break;
    }
    push_value(code, op::pop, pop_size);
}

// This should NOT pop data from the stack like handle_loop_exit since the
// op::ret op code does this for us.
auto variable_manager::handle_function_exit(std::vector<std::byte>& code) -> void
{
    panic_if(!in_function(), "cannot call handle_function_exit in the global scope");
    for (const auto& scope : d_scopes | std::views::reverse) {
        delete_arenas_in_scope(code, scope, d_local);
    }
}

void variable_manager::pop_scope(std::vector<std::byte>& code) {
    delete_arenas_in_scope(code, d_scopes.back(), d_local);
    const auto& scope = d_scopes.back();
    const auto size = scope.next - scope.start;
    d_scopes.pop_back();
    if (size > 0) push_value(code, op::pop, size); 
}

}