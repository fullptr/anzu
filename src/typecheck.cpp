#include "typecheck.hpp"
#include "parser.hpp"
#include "vocabulary.hpp"

#include <ranges>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

template <typename... Args>
[[noreturn]] void type_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

template <typename... Args>
[[noreturn]] void type_error(const parser_context& ctx, std::string_view msg, Args&&... args)
{
    type_error(ctx.tokens.curr(), msg, std::forward<Args>(args)...);
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

}

auto fetch_function_signature(
    const parser_context& ctx, const std::string& function_name
)
    -> function_signature
{
    const auto& scope = ctx.current_scope();
    if (auto it = scope.functions.find(function_name); it != scope.functions.end()) {
        return it->second;
    }

    if (anzu::is_builtin(function_name)) {
        return anzu::fetch_builtin(function_name).sig;
    }

    type_error(ctx, "could not find function '{}'", function_name);
}

auto type_check_function_call(
    const parser_context& ctx,
    const std::string& function_name,
    std::span<const node_expr_ptr> args
)
    -> void
{
    const auto sig = fetch_function_signature(ctx, function_name);
    if (sig.args.size() != args.size()) {
        type_error(
            ctx, "function '{}' expected {} args, got {}",
            function_name, sig.args.size(), args.size()
        );
    }

    for (std::size_t idx = 0; idx != sig.args.size(); ++idx) {
        const auto& expected = sig.args.at(idx).type;
        const auto& actual = type_of_expr(ctx, *args[idx]);
        if (expected != tk_any && actual != tk_any && expected != actual) {
            type_error(
                ctx, "invalid function call, arg {} expects type {}, got {}\n",
                idx, expected, actual
            );
        }
    }
}

auto type_of_expr(const parser_context& ctx, const node_expr& expr) -> std::string
{
    return std::visit(overloaded {
        [&](const node_literal_expr& node) {
            return type_of(node.value);
        },
        [&](const node_variable_expr& node) {
            const auto& top = ctx.scopes.top();
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

}