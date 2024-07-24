#include "ast.hpp"
#include "utility/common.hpp"

#include <functional>
#include <ranges>

namespace anzu {

auto print_node(const node_expr& root, int indent) -> void
{
    const auto spaces = std::string(4 * indent, ' ');
    std::visit(overloaded {
        [&](const node_literal_i32_expr& node) {
            print("{}Literal (i32): {}\n", spaces, node.value);
        },
        [&](const node_literal_i64_expr& node) {
            print("{}Literal (i64): {}\n", spaces, node.value);
        },
        [&](const node_literal_u64_expr& node) {
            print("{}Literal (u64): {}\n", spaces, node.value);
        },
        [&](const node_literal_f64_expr& node) {
            print("{}Literal (f64): {}\n", spaces, node.value);
        },
        [&](const node_literal_char_expr& node) {
            print("{}Literal (char): {}\n", spaces, node.value);
        },
        [&](const node_literal_bool_expr& node) {
            print("{}Literal (bool): {}\n", spaces, node.value);
        },
        [&](const node_literal_null_expr&) {
            print("{}Literal (null): null\n", spaces);
        },
        [&](const node_literal_nullptr_expr&) {
            print("{}Literal (nullptr): nullptr\n", spaces);
        },
        [&](const node_literal_string_expr& node) {
            print("{}Literal (string-literal): {}\n", spaces, node.value);
        },
        [&](const node_name_expr& node) {
            print("{}Name: {}\n", spaces, node.name);
        },
        [&](const node_field_expr& node) {
            print("{}Field: \n", spaces);
            print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            print("{}- Field: {}\n", spaces, node.field_name);
        },
        [&](const node_unary_op_expr& node) {
            print("{}UnaryOp: \n", spaces);
            print("{}- Op: {}\n", spaces, node.token.text);
            print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_binary_op_expr& node) {
            print("{}BinaryOp: \n", spaces);
            print("{}- Op: {}\n", spaces, node.token.text);
            print("{}- Lhs:\n", spaces);
            print_node(*node.lhs, indent + 1);
            print("{}- Rhs:\n", spaces);
            print_node(*node.rhs, indent + 1);
        },
        [&](const node_call_expr& node) {
            print("{}Call:\n", spaces);
            print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        },
        [&](const node_member_call_expr& node) {
            print("{}MemberCall:\n", spaces);
            print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            print("{}- FunctionName: {}\n", spaces, node.function_name);
            if (node.template_type) {
                print("{}- TemplateType: {}\n", spaces, *node.template_type);
            }
            print("{}- OtherArgs:\n", spaces);
            for (const auto& arg : node.other_args) {
                print_node(*arg, indent + 1);
            }
        },
        [&](const node_array_expr& node) {
            print("{}Array:\n", spaces);
            print("{}- Elements:\n", spaces);
            for (const auto& element : node.elements) {
                print_node(*element, indent + 1);
            }
        },
        [&](const node_repeat_array_expr& node) {
            print("{}Array:\n", spaces);
            print("{}- Element:\n", spaces);
            print_node(*node.value, indent + 1);
            print("{}- Count: {}\n", spaces, node.size);
        },
        [&](const node_addrof_expr& node) {
            print("{}AddrOf:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_deref_expr& node) {
            print("{}Deref:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_sizeof_expr& node) {
            print("{}SizeOf:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_subscript_expr& node) {
            print("{}Subscript:\n", spaces);
            print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            print("{}- Index:\n", spaces);
            print_node(*node.index, indent + 1);
        },
        [&](const node_span_expr& node) {
            print("{}Span:\n", spaces);
            print_node(*node.expr, indent + 1);
            if (node.lower_bound) {
                print("{}LowerBound:\n", spaces);
                print_node(*node.lower_bound, indent + 1);
            }
            if (node.upper_bound) {
                print("{}LowerBound:\n", spaces);
                print_node(*node.upper_bound, indent + 1);
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
        [&](const node_loop_stmt& node) {
            print("{}Loop:\n", spaces);
            print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_while_stmt& node) {
            print("{}While:\n", spaces);
            print("{}- Condition:\n", spaces);
            print_node(*node.condition, indent + 1);
            print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_for_stmt& node) {
            print("{}For (name={}):\n", spaces, node.name);
            print("{}- Iter:\n", spaces);
            print_node(*node.iter, indent + 1);
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
        [&](const node_struct_stmt& node) {
            print("{}Struct:\n", spaces);
            print("{}- Name: {}\n", spaces, node.name);
            print("{}- Fields:\n", spaces);
            for (const auto& field : node.fields) {
                print("{}  - {}: {}\n", spaces, field.name, *field.type);
            }
            print("{}- MemberFunctions:\n", spaces);
            for (const auto& function : node.functions) {
                print_node(*function, indent + 1);
            }
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
            print("{}- AddConst: {}\n", spaces, node.add_const);
            print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_arena_declaration_stmt& node) {
            print("{}ArenaDeclaration:\n", spaces);
            print("{}- Name: {}\n", spaces, node.name);
        },
        [&](const node_assignment_stmt& node) {
            print("{}Assignment:\n", spaces);
            print("{}- Name:\n", spaces);
            print_node(*node.position, indent + 1);
            print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_function_def_stmt& node) {
            print("{}Function: {} (", spaces, node.name);
            print_comma_separated(node.sig.params, [](const auto& arg) {
                return std::format("{}: {}", arg.name, *arg.type);
            });
            print(") -> {}\n", *node.sig.return_type);
            print_node(*node.body, indent + 1);
        },
        [&](const node_member_function_def_stmt& node) {
            print("{}MemberFunction: {}::{} (", spaces, node.struct_name, node.function_name);
            print_comma_separated(node.sig.params, [](const auto& arg) {
                return std::format("{}: {}", arg.name, *arg.type);
            });
            print(") -> {}\n", *node.sig.return_type);
            print_node(*node.body, indent + 1);
        },
        [&](const node_expression_stmt& node) {
            print("{}Expression:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_return_stmt& node) {
            print("{}Return:\n", spaces);
            print_node(*node.return_value, indent + 1);
        },
        [&](const node_assert_stmt& node) {
            print("{}Assert:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_print_stmt& node) {
            print("{}Print:\n", spaces);
            print("{}- Message: {}\n", spaces, node.message);
            print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        }
    }, root);
}

auto print_node(const anzu::node_type& root, int indent) -> void
{
    const auto spaces = std::string(4 * indent, ' ');
    std::visit(overloaded {
        [&](const node_named_type& node) {
            print("{}NamedType:\n {}", spaces, node.type);
        },
        [&](const node_expr_type& node) {
            print("{}ExprType:\n", spaces);
            print_node(*node.expr, indent + 1);
        }
    }, root);
}

auto is_lvalue_expr(const node_expr& expr) -> bool
{
    return std::holds_alternative<node_name_expr>(expr)
        || std::holds_alternative<node_field_expr>(expr)
        || std::holds_alternative<node_deref_expr>(expr)
        || std::holds_alternative<node_subscript_expr>(expr);
}

auto is_rvalue_expr(const node_expr& expr) -> bool
{
    return !is_lvalue_expr(expr);
}

auto to_string(const node_type& node) -> std::string
{
    return std::visit(overloaded{
        [&](const node_named_type& n) {
            return to_string(n.type);
        },
        [&](const node_expr_type& n) {
            return std::string{"typeof(<expr>)"};
        }
    }, node);
}

}