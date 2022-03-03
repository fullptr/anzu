#include "optimiser.hpp"
#include "object.hpp"
#include "utility/overloaded.hpp"

#include <optional>

namespace anzu {
namespace {

auto evaluate_bin_op(
    const object_def& lhs, const object_def& rhs, std::string_view op
)
    -> object_def
{
    if (lhs.data.size() != 1 || rhs.data.size() != 1) {
        anzu::print("optimising operations of objects with block size != 1 not currently supported\n");
        std::exit(1);
    }

    const auto& lhsv = lhs.data.front();
    const auto& rhsv = rhs.data.front();

    auto result = object_def{};

    if (op == "+")       {
        result.data = { block{lhsv +  rhsv} };
        result.type = lhs.type;
    }
    else if (op == "-")  {
        result.data = { block{lhsv -  rhsv} };
        result.type = lhs.type;
    }
    else if (op == "*")  {
        result.data = { block{lhsv *  rhsv} };
        result.type = lhs.type;
    }
    else if (op == "/")  {
        result.data = { block{lhsv /  rhsv} };
        result.type = lhs.type;
    }
    else if (op == "%")  {
        result.data = { block{lhsv %  rhsv} };
        result.type = lhs.type;
    }
    else if (op == "<")  {
        result.data = { block{lhsv <  rhsv} };
        result.type = bool_type();
    }
    else if (op == "<=") {
        result.data = { block{lhsv <= rhsv} };
        result.type = bool_type();
    }
    else if (op == ">")  {
        result.data = { block{lhsv >  rhsv} };
        result.type = bool_type();
    }
    else if (op == ">=") {
        result.data = { block{lhsv >= rhsv} }; 
        result.type = bool_type();
    }
    else if (op == "==") {
        result.data = { block{lhsv == rhsv} };
        result.type = bool_type();
    }
    else if (op == "!=") {
        result.data = { block{lhsv != rhsv} };
        result.type = bool_type();
    }
    else if (op == "||") {
        result.data = { block{lhsv || rhsv} };
        result.type = bool_type();
    }
    else if (op == "&&") {
        result.data = { block{lhsv && rhsv} };
        result.type = bool_type();
    }
    else {
        anzu::print("syntax error: unknown binary operator: '{}'\n", op);
        std::exit(1);
    }

    return result;
}

auto evaluate_const_expressions_recurse(node_expr& expr) -> std::optional<object_def>
{
    using return_type = std::optional<object_def>;
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
                auto val = evaluate_bin_op(lhs.value(), rhs.value(), node.token.text);
                expr.emplace<anzu::node_literal_expr>(val);
                return val;
            }
            return std::nullopt;
        },
        [&](const node_list_expr& node) -> return_type {
            return std::nullopt; // Cannot do this for now, since lists store blocks, not objects
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
        [](node_declaration_stmt& stmt) {
            evaluate_const_expressions(*stmt.expr);
        },
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