#include "optimiser.hpp"
#include "object.hpp"

#include <optional>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

auto evaluate_bin_op(
    const anzu::object& lhs, const anzu::object& rhs, std::string_view op
)
    -> anzu::object
{
    if      (op == "+")  { return lhs + rhs; }
    else if (op == "-")  { return lhs - rhs; }
    else if (op == "*")  { return lhs * rhs; }
    else if (op == "/")  { return lhs / rhs; }
    else if (op == "%")  { return lhs % rhs; }
    else if (op == "<")  { return lhs < rhs; }
    else if (op == "<=") { return lhs <= rhs; }
    else if (op == ">")  { return lhs > rhs; }
    else if (op == ">=") { return lhs >= rhs; }
    else if (op == "==") { return lhs == rhs; }
    else if (op == "!=") { return lhs != rhs; }
    else if (op == "||") { return lhs || rhs; }
    else if (op == "&&") { return lhs && rhs; }
    else {
        anzu::print("syntax error: unknown binary operator: '{}'\n", op);
        std::exit(1);
    }
}

auto evaluate_const_expressions_recurse(node_expr& expr) -> std::optional<anzu::object>
{
    using return_type = std::optional<anzu::object>;
    return std::visit(overloaded {
        [](const node_literal_expr& node) -> return_type {
            return node.value;
        },
        [](const node_variable_expr& node) -> return_type {
            return std::nullopt;
        },
        [](const node_function_call_expr& node) -> return_type {
            return std::nullopt;
        },
        [&](const node_bin_op_expr& node) -> return_type {
            auto lhs = evaluate_const_expressions_recurse(*node.lhs);
            auto rhs = evaluate_const_expressions_recurse(*node.rhs);
            if (lhs.has_value() && rhs.has_value()) {
                auto val = evaluate_bin_op(lhs.value(), rhs.value(), node.op.text);
                expr.emplace<anzu::node_literal_expr>(val);
                return val;
            }
            return std::nullopt;
        }
    }, expr);
}

auto evaluate_const_expressions(node_expr& expr) -> void
{
    auto obj = evaluate_const_expressions_recurse(expr);
    if (obj.has_value()) {
        expr.emplace<anzu::node_literal_expr>(obj.value());
    }
}

auto evaluate_const_expressions(node_stmt& tree) -> void
{
    std::visit(overloaded {
        [](node_sequence_stmt& stmt) {
            for (auto& sub_stmt : stmt.sequence) {
                evaluate_const_expressions(*sub_stmt);
            }
        },
        [](node_while_stmt& stmt) {
            evaluate_const_expressions(*stmt.condition);
            evaluate_const_expressions(*stmt.body);
        },
        [](node_if_stmt& stmt) {
            evaluate_const_expressions(*stmt.condition);
            evaluate_const_expressions(*stmt.body);
            if (stmt.else_body) {
                evaluate_const_expressions(*stmt.else_body);
            }
        },
        [](node_for_stmt& stmt) {
            evaluate_const_expressions(*stmt.container);
            evaluate_const_expressions(*stmt.body);
        },
        [](node_break_stmt&) {},
        [](node_continue_stmt&) {},
        [](node_assignment_stmt& stmt) {
            evaluate_const_expressions(*stmt.expr);
        },
        [](node_function_def_stmt& stmt) {
            evaluate_const_expressions(*stmt.body);
        },
        [](node_function_call_stmt& stmt) {
            for (auto& arg : stmt.args) {
                evaluate_const_expressions(*arg);
            }
        },
        [](node_return_stmt& stmt) {
            evaluate_const_expressions(*stmt.return_value);
        }
    }, tree);
}

}

auto optimise_ast(node_stmt& tree) -> void
{
    evaluate_const_expressions(tree);
}

}