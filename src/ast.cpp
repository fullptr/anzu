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
            std::print("{}Literal (i32): {}\n", spaces, node.value);
        },
        [&](const node_literal_i64_expr& node) {
            std::print("{}Literal (i64): {}\n", spaces, node.value);
        },
        [&](const node_literal_u64_expr& node) {
            std::print("{}Literal (u64): {}\n", spaces, node.value);
        },
        [&](const node_literal_f64_expr& node) {
            std::print("{}Literal (f64): {}\n", spaces, node.value);
        },
        [&](const node_literal_char_expr& node) {
            std::print("{}Literal (char): {}\n", spaces, node.value);
        },
        [&](const node_literal_bool_expr& node) {
            std::print("{}Literal (bool): {}\n", spaces, node.value);
        },
        [&](const node_literal_null_expr&) {
            std::print("{}Literal (null): null\n", spaces);
        },
        [&](const node_literal_nullptr_expr&) {
            std::print("{}Literal (nullptr): nullptr\n", spaces);
        },
        [&](const node_literal_string_expr& node) {
            std::print("{}Literal (string-literal): {}\n", spaces, node.value);
        },
        [&](const node_name_expr& node) {
            std::print("{}Name: {}\n", spaces, node.name);
            std::print("{}- Templates:\n", spaces);
            for (const auto& arg : node.templates) {
                print_node(*arg, indent + 1);
            }
        },
        [&](const node_field_expr& node) {
            std::print("{}Field: \n", spaces);
            std::print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            std::print("{}- Field: {}\n", spaces, node.field_name);
            std::print("{}- Templates:\n", spaces);
            for (const auto& arg : node.templates) {
                print_node(*arg, indent + 1);
            }
        },
        [&](const node_unary_op_expr& node) {
            std::print("{}UnaryOp: \n", spaces);
            std::print("{}- Op: {}\n", spaces, node.token.text);
            std::print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_binary_op_expr& node) {
            std::print("{}BinaryOp: \n", spaces);
            std::print("{}- Op: {}\n", spaces, node.token.text);
            std::print("{}- Lhs:\n", spaces);
            print_node(*node.lhs, indent + 1);
            std::print("{}- Rhs:\n", spaces);
            print_node(*node.rhs, indent + 1);
        },
        [&](const node_call_expr& node) {
            std::print("{}Call:\n", spaces);
            std::print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            std::print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        },
        [&](const node_array_expr& node) {
            std::print("{}Array:\n", spaces);
            std::print("{}- Elements:\n", spaces);
            for (const auto& element : node.elements) {
                print_node(*element, indent + 1);
            }
        },
        [&](const node_repeat_array_expr& node) {
            std::print("{}Array:\n", spaces);
            std::print("{}- Element:\n", spaces);
            print_node(*node.value, indent + 1);
            std::print("{}- Count: {}\n", spaces, node.size);
        },
        [&](const node_addrof_expr& node) {
            std::print("{}AddrOf:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_deref_expr& node) {
            std::print("{}Deref:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_sizeof_expr& node) {
            std::print("{}SizeOf:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_subscript_expr& node) {
            std::print("{}Subscript:\n", spaces);
            std::print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            std::print("{}- Index:\n", spaces);
            print_node(*node.index, indent + 1);
        },
        [&](const node_span_expr& node) {
            std::print("{}Span:\n", spaces);
            std::print("{}- Expr:\n", spaces);
            print_node(*node.expr, indent + 1);
            if (node.lower_bound) {
                std::print("{}- LowerBound:\n", spaces);
                print_node(*node.lower_bound, indent + 1);
            }
            if (node.upper_bound) {
                std::print("{}- LowerBound:\n", spaces);
                print_node(*node.upper_bound, indent + 1);
            }
        },
        [&](const node_typeof_expr& node) {
            std::print("{}TypeOf\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_function_ptr_type_expr& node) {
            std::print("{}FunctionPtrType:\n", spaces);
            std::print("{}- Params:\n", spaces);
            for (const auto& param : node.params) {
                print_node(*param, indent + 1);
            }
            std::print("{}- ReturnType:\n", spaces);
            print_node(*node.return_type, indent + 1);
        },
        [&](const node_const_expr& node) {
            std::print("{}Const:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_new_expr& node) {
            std::print("{}New:\n", spaces);
            std::print("{}- Arena:\n", spaces);
            print_node(*node.arena);
            std::print("{}- Count:\n", spaces);
            print_node(*node.count);
            std::print("{}- Expr:\n", spaces);
            print_node(*node.expr);
        }
    }, root);
}

auto print_node(const node_stmt& root, int indent) -> void
{
    const auto spaces = std::string(4 * indent, ' ');
    std::visit(overloaded {
        [&](const node_sequence_stmt& node) {
            std::print("{}Sequence:\n", spaces);
            for (const auto& seq_node : node.sequence) {
                print_node(*seq_node, indent + 1);
            }
        },
        [&](const node_loop_stmt& node) {
            std::print("{}Loop:\n", spaces);
            std::print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_while_stmt& node) {
            std::print("{}While:\n", spaces);
            std::print("{}- Condition:\n", spaces);
            print_node(*node.condition, indent + 1);
            std::print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_for_stmt& node) {
            std::print("{}For (name={}):\n", spaces, node.name);
            std::print("{}- Iter:\n", spaces);
            print_node(*node.iter, indent + 1);
            std::print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_if_stmt& node) {
            std::print("{}If:\n", spaces);
            std::print("{}- Condition:\n", spaces);
            print_node(*node.condition, indent + 1);
            std::print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
            if (node.else_body) {
                std::print("{}- Else:\n", spaces);
                print_node(*node.else_body, indent + 1);
            }
        },
        [&](const node_struct_stmt& node) {
            std::print("{}Struct:\n", spaces);
            std::print("{}- Name: {}\n", spaces, node.name);
            std::print("{}- Fields:\n", spaces);
            for (const auto& field : node.fields) {
                std::print("    {}:\n", field.name);
                print_node(*field.type, indent + 1);
            }
            std::print("{}- MemberFunctions:\n", spaces);
            for (const auto& function : node.functions) {
                print_node(*function, indent + 1);
            }
        },
        [&](const node_break_stmt& node) {
            std::print("{}Break\n", spaces);
        },
        [&](const node_continue_stmt& node) {
            std::print("{}Continue\n", spaces);
        },
        [&](const node_declaration_stmt& node) {
            std::print("{}Declaration:\n", spaces);
            std::print("{}- Name: {}\n", spaces, node.name);
            if (node.explicit_type) {
                std::print("{}- ExplicitType:\n", spaces);
                print_node(*node.explicit_type, indent + 1);
            }
            std::print("{}- AddConst: {}\n", spaces, node.add_const);
            std::print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_arena_declaration_stmt& node) {
            std::print("{}ArenaDeclaration:\n", spaces);
            std::print("{}- Name: {}\n", spaces, node.name);
        },
        [&](const node_assignment_stmt& node) {
            std::print("{}Assignment:\n", spaces);
            std::print("{}- Name:\n", spaces);
            print_node(*node.position, indent + 1);
            std::print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_member_function_def_stmt& node) {
            if (node.struct_name.empty()) {
                std::print("{}Function: {}", spaces, node.function_name);
            } else {
                std::print("{}Function: {}::{}", spaces, node.struct_name, node.function_name);
            }
            if (!node.template_types.empty()) {
                std::print("!(");
                print_comma_separated(node.template_types, [](const auto& arg) {
                    return arg;
                });
                std::print(")");
            }
            std::print("\n");
            std::print("{}- FunctionArguments:\n", spaces);
            for (const auto& param : node.sig.params) {
                std::print("    {}:\n", param.name);
                print_node(*param.type, indent + 1);
            }
            std::print("{}- ReturnType:\n", spaces);
            print_node(*node.sig.return_type, indent + 1);
            std::print("{}- Body\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [&](const node_expression_stmt& node) {
            std::print("{}Expression:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_return_stmt& node) {
            std::print("{}Return:\n", spaces);
            print_node(*node.return_value, indent + 1);
        },
        [&](const node_assert_stmt& node) {
            std::print("{}Assert:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [&](const node_print_stmt& node) {
            std::print("{}Print:\n", spaces);
            std::print("{}- Message: {}\n", spaces, node.message);
            std::print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
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

}