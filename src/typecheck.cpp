#include "typecheck.hpp"
#include "vocabulary.hpp"

#include <ranges>
#include <unordered_map>
#include <stack>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

struct typecheck_scope
{
    std::unordered_map<std::string, function_signature> functions;
    std::unordered_map<std::string, std::string>        variables;
};

using typecheck_context = std::stack<typecheck_scope>;

template <typename... Args>
[[noreturn]] void type_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

template <typename... Args>
[[noreturn]] void type_error(std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", "?", "?", formatted_msg);
    std::exit(1);
}

auto type_of_bin_op(
    std::string_view lhs, std::string_view rhs, const token& op_token
)
    -> std::string
{
    const auto op = op_token.text;
    const auto invalid_expr = [=]() {
        type_error(op_token, "could not evaluate '{} {} {}'", lhs, op, rhs);
    };

    if (lhs == tk_any || rhs == tk_any) {
        return std::string{tk_any};
    }

    if (lhs != rhs) {
        invalid_expr();
    }

    if (lhs == tk_list || lhs == tk_null_type) { // No support for having these in binary ops.
        invalid_expr();
    }

    if (lhs == tk_str) {
        // Allowed: string concatenation and equality check
        if (op == tk_add) {
            return std::string{tk_str};
        }
        if (op == tk_eq || op == tk_ne) {
            return std::string{tk_bool};
        }
        invalid_expr();
    }

    if (lhs == tk_bool) {
        if (op == tk_or || op == tk_and || op == tk_eq || op == tk_ne) {
            return std::string{tk_bool};
        }
        invalid_expr();
    }

    if (is_comparison(op)) {
        return std::string{tk_bool};
    }
    return std::string{tk_int};
}

auto fetch_function_signature(
    const typecheck_context& ctx, const std::string& function_name
)
    -> function_signature
{
    const auto& scope = ctx.top();
    if (auto it = scope.functions.find(function_name); it != scope.functions.end()) {
        return it->second;
    }

    if (anzu::is_builtin(function_name)) {
        return anzu::fetch_builtin(function_name).sig;
    }

    type_error("could not find function '{}'", function_name);
}

auto type_of_expr(const typecheck_context& ctx, const node_expr& expr) -> std::string
{
    return std::visit(overloaded {
        [&](const node_literal_expr& node) {
            return type_of(node.value);
        },
        [&](const node_variable_expr& node) {
            const auto& top = ctx.top();
            return top.variables.at(node.name);
        },
        [&](const node_function_call_expr& node) {
            const auto& func_def = fetch_function_signature(ctx, node.function_name);
            return func_def.return_type;
        },
        [&](const node_bin_op_expr& node) {
            return type_of_bin_op(
                type_of_expr(ctx, *node.lhs), type_of_expr(ctx, *node.rhs), node.op
            );
        }
    }, expr);
};

void verify_expression_type(typecheck_context& ctx, const node_expr& expr, std::string_view expected)
{
    const auto actual = type_of_expr(ctx, expr);
    if (actual != tk_any && actual != expected) {
        type_error("expected '{}', got '{}'", expected, actual);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void;

auto typecheck_node(typecheck_context& ctx, const node_sequence_stmt& node) -> void
{
    for (const auto& child : node.sequence) {
        typecheck_node(ctx, *child);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_while_stmt& node) -> void
{
    verify_expression_type(ctx, *node.condition, tk_bool);
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_if_stmt& node) -> void
{
    verify_expression_type(ctx, *node.condition, tk_bool);
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_for_stmt& node) -> void
{
    ctx.top().variables[node.var] = tk_any; // Can't know type yet :(
    verify_expression_type(ctx, *node.container, tk_list);
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_break_stmt&) -> void
{
}

auto typecheck_node(typecheck_context& ctx, const node_continue_stmt&) -> void
{
}

auto typecheck_node(typecheck_context& ctx, const node_assignment_stmt& node) -> void
{
    ctx.top().variables[node.name] = type_of_expr(ctx, *node.expr);
}

auto typecheck_node(typecheck_context& ctx, const node_function_def_stmt& node) -> void
{
    ctx.top().functions[node.name] = node.sig; // Make name available in outer scope

    ctx.emplace();
    for (const auto& arg : node.sig.args) {
        // TODO: Check that arg.type is a valid type.
        ctx.top().variables[arg.name] = arg.type;
    }
    ctx.top().variables["$return"] = node.sig.return_type; // Expose the return type for children
    ctx.top().functions[node.name] = node.sig;             // Make available for recursion
    typecheck_node(ctx, *node.body);
    ctx.pop();
}

auto typecheck_node(typecheck_context& ctx, const node_function_call_stmt& node) -> void
{
    const auto sig = fetch_function_signature(ctx, node.function_name);
    if (sig.args.size() != node.args.size()) {
        type_error(
            "function '{}' expected {} args, got {}",
            node.function_name, sig.args.size(), node.args.size()
        );
    }

    for (std::size_t idx = 0; idx != sig.args.size(); ++idx) {
        const auto& expected = sig.args.at(idx).type;
        const auto& actual = type_of_expr(ctx, *node.args[idx]);
        if (expected != tk_any && actual != tk_any && expected != actual) {
            type_error(
                "invalid function call, arg {} expects type {}, got {}\n",
                idx, expected, actual
            );
        }
    }
}

auto typecheck_node(typecheck_context& ctx, const node_return_stmt& node)
{
    const auto& return_type = ctx.top().variables.at("$return");
    verify_expression_type(ctx, *node.return_value, return_type);
}

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void
{
    std::visit([&](const auto& n) { typecheck_node(ctx, n); }, node);
}

}

auto type_of(const anzu::object& object) -> std::string
{
    if (object.is<int>()) {
        return std::string{tk_int};
    }
    if (object.is<bool>()) {
        return std::string{tk_bool};
    }
    if (object.is<std::string>()) {
        return std::string{tk_str};
    }
    if (object.is<object_list>()) {
        return std::string{tk_list};
    }
    if (object.is<object_null>()) {
        return std::string{tk_null_type};
    }
    return std::string{tk_any};
}

auto typecheck_ast(const node_stmt_ptr& ast) -> void
{
    auto ctx = typecheck_context{};
    ctx.emplace(); // Global scope
    typecheck_node(ctx, *ast);
}

}