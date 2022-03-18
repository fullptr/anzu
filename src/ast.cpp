#include "ast.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <functional>
#include <ranges>

namespace anzu {

auto print_node(const node_expr& root, int indent) -> void
{
    const auto spaces = std::string(4 * indent, ' ');
    std::visit(overloaded {
        [&](const node_literal_expr& node) {
            print("{}Literal: {}\n", spaces, node.value);
        },
        [&](const node_variable_expr& node) {
            print("{}Variable: {}\n", spaces, node.name);
        },
        [&](const node_field_expr& node) {
            print("{}Field: \n", spaces);
            print("{}- Expr:\n", spaces);
            print_node(*node.expression, indent + 1);
            print("{}- Field: {}\n", spaces, node.field_name);
        },
        [&](const node_bin_op_expr& node) {
            print("{}BinOp: \n", spaces);
            print("{}- Op: {}\n", spaces, node.token.text);
            print("{}- Lhs:\n", spaces);
            print_node(*node.lhs, indent + 1);
            print("{}- Rhs:\n", spaces);
            print_node(*node.rhs, indent + 1);
        },
        [&](const node_function_call_expr& node) {
            print("{}FunctionCall (Expr): {}\n", spaces, node.function_name);
            print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        },
        [&](const node_list_expr& node) {
            print("{}List (Expr):\n", spaces);
            print("{}- Elements:\n", spaces);
            for (const auto& element : node.elements) {
                print_node(*element, indent + 1);
            }
        }
    }, root);
}

auto print_node(const node_stmt& root, int indent) -> void
{
    const auto spaces = std::string(4 * indent, ' ');
    std::visit(overloaded {
        [&](const node_sequence_stmt& node) {
            print("{}Sequence:\n", spaces);
            for (const auto& seq_node : node.sequence) {
                print_node(*seq_node, indent + 1);
            }
        },
        [&](const node_while_stmt& node) {
            print("{}While:\n", spaces);
            print("{}- Condition:\n", spaces);
            print_node(*node.condition, indent + 1);
            print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_if_stmt& node) {
            print("{}If:\n", spaces);
            print("{}- Condition:\n", spaces);
            print_node(*node.condition, indent + 1);
            print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
            if (node.else_body) {
                print("{}- Else:\n", spaces);
                print_node(*node.else_body, indent + 1);
            }
        },
        [&](const node_for_stmt& node) {
            print("{}For:\n", spaces);
            print("{}- Bind: {}\n",spaces, node.var);
            print("{}- Container:\n",spaces);
            print_node(*node.container, indent + 1);
            print("{}- Body:\n",spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_break_stmt& node) {
            print("{}Break\n", spaces);
        },
        [&](const node_continue_stmt& node) {
            print("{}Continue\n", spaces);
        },
        [&](const node_declaration_stmt& node) {
            print("{}Declaration:\n", spaces);
            print("{}- Name: {}\n", spaces, node.name);
            print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_assignment_stmt& node) {
            print("{}Assignment:\n", spaces);
            print("{}- Name: {}\n", spaces, node.name);
            print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_field_assignment_stmt& node) {
            print("{}FieldAssignment:\n", spaces);
            print("{}- Name: {}\n", spaces, node.name);
            print("{}- Fields: {}\n", spaces, format_comma_separated(node.fields));
            print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_function_def_stmt& node) {
            print("{}Function: {} (", spaces, node.name);
            print_comma_separated(node.sig.args, [](const auto& arg) {
                return std::format("{}: {}", arg.name, arg.type);
            });
            print(") -> {}\n", node.sig.return_type);
            print_node(*node.body, indent + 1);
        },
        [&](const node_function_call_stmt& node) {
            print("{}FunctionCall (Stmt): {}\n", spaces, node.function_name);
            print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        },
        [&](const node_return_stmt& node) {
            print("{}Return:\n", spaces);
            print_node(*node.return_value, indent + 1);
        }
    }, root);
}

}