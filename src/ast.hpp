#pragma once
#include "object.hpp"
#include "functions.hpp"
#include "token.hpp"

#include <variant>
#include <vector>
#include <memory>

namespace anzu {

struct node_expr;
using node_expr_ptr = std::shared_ptr<node_expr>;

struct node_type;
using node_type_ptr = std::shared_ptr<node_type>;

struct node_stmt;
using node_stmt_ptr = std::shared_ptr<node_stmt>;

struct node_named_type
{
    type_name type;

    anzu::token token;
};

struct node_expr_type
{
    node_expr_ptr expr;

    anzu::token token;
};

struct node_type : std::variant<
    node_named_type,
    node_expr_type>
{
};

struct node_parameter
{
    std::string   name;
    node_type_ptr type;
};

struct node_signature
{
    std::vector<node_parameter> params;
    node_type_ptr               return_type;
};

struct node_literal_expr
{
    anzu::object value;

    anzu::token token;
};

struct node_name_expr
{
    node_type_ptr struct_name = nullptr;
    std::string name;

    anzu::token token;
};

struct node_field_expr
{
    node_expr_ptr expr;
    std::string   field_name;

    anzu::token token;
};

struct node_unary_op_expr
{
    node_expr_ptr expr;

    anzu::token token;
};

struct node_binary_op_expr
{
    node_expr_ptr lhs;
    node_expr_ptr rhs;

    anzu::token token;
};

struct node_call_expr
{
    node_expr_ptr expr;
    std::vector<node_expr_ptr> args;

    anzu::token token;
};

struct node_list_expr
{
    std::vector<node_expr_ptr> elements;

    anzu::token token;
};

struct node_repeat_list_expr
{
    node_expr_ptr value;
    std::size_t   size;

    anzu::token token;
};

struct node_addrof_expr
{
    node_expr_ptr expr;

    anzu::token token;
};

struct node_deref_expr
{
    node_expr_ptr expr;
    
    anzu::token token;
};

struct node_sizeof_expr
{
    node_expr_ptr expr;

    anzu::token token;
};

struct node_subscript_expr
{
    node_expr_ptr expr;
    node_expr_ptr index;
    
    anzu::token token;
};

struct node_new_expr
{
    node_type_ptr type;
    node_expr_ptr size;
    
    anzu::token token;
};

struct node_span_expr
{
    node_expr_ptr expr;
    node_expr_ptr lower_bound;
    node_expr_ptr upper_bound;
    
    anzu::token token;
};

struct node_expr : std::variant<
    // Rvalue expressions
    node_literal_expr,
    node_unary_op_expr,
    node_binary_op_expr,
    node_call_expr,
    node_list_expr,
    node_repeat_list_expr,
    node_addrof_expr,
    node_sizeof_expr,
    node_new_expr,

    // Lvalue expressions
    node_name_expr,
    node_field_expr,
    node_deref_expr,
    node_subscript_expr,
    node_span_expr>
{
};

struct node_sequence_stmt
{
    std::vector<node_stmt_ptr> sequence;

    anzu::token token;
};

struct node_loop_stmt
{
    node_stmt_ptr body;

    anzu::token token;
};

struct node_while_stmt
{
    node_expr_ptr condition;
    node_stmt_ptr body;

    anzu::token token;
};

struct node_for_stmt
{
    std::string name;
    node_expr_ptr iter;
    node_stmt_ptr body;

    anzu::token token;
};

struct node_if_stmt
{
    node_expr_ptr condition;
    node_stmt_ptr body;
    node_stmt_ptr else_body;

    anzu::token token;
};

struct node_struct_stmt
{
    std::string                 name;
    std::vector<node_parameter> fields;
    std::vector<node_stmt_ptr>  functions;

    anzu::token token;
};

struct node_break_stmt
{
    anzu::token token;
};

struct node_continue_stmt
{
    anzu::token token;
};

struct node_declaration_stmt
{
    std::string   name;
    node_expr_ptr expr;

    anzu::token token;
};

struct node_assignment_stmt
{
    node_expr_ptr position;
    node_expr_ptr expr;

    anzu::token token;
};

struct node_function_def_stmt
{
    std::string    name;
    node_signature sig;
    node_stmt_ptr  body;

    anzu::token token;
};

struct node_member_function_def_stmt
{
    std::string    struct_name;
    std::string    function_name;
    node_signature sig;
    node_stmt_ptr  body;

    anzu::token token;
};

struct node_expression_stmt
{
    node_expr_ptr expr;

    anzu::token token;
};

struct node_return_stmt
{
    node_expr_ptr return_value;

    anzu::token token;
};

struct node_delete_stmt
{
    node_expr_ptr expr;
    node_expr_ptr size;

    anzu::token token;
};

struct node_assert_stmt
{
    node_expr_ptr expr;

    anzu::token token;
};

struct node_stmt : std::variant<
    node_sequence_stmt,
    node_loop_stmt,
    node_while_stmt,
    node_for_stmt,
    node_if_stmt,
    node_struct_stmt,
    node_break_stmt,
    node_continue_stmt,
    node_declaration_stmt,
    node_assignment_stmt,
    node_member_function_def_stmt,
    node_function_def_stmt,
    node_expression_stmt,
    node_return_stmt,
    node_delete_stmt,
    node_assert_stmt>
{
};

auto is_lvalue_expr(const node_expr& expr) -> bool;
auto is_rvalue_expr(const node_expr& expr) -> bool;

auto print_node(const anzu::node_expr& root, int indent = 0) -> void;
auto print_node(const anzu::node_stmt& root, int indent = 0) -> void;
auto print_node(const anzu::node_type& root, int indent = 0) -> void;

auto to_string(const node_type& node) -> std::string;

}

template <> struct std::formatter<anzu::node_type> : std::formatter<std::string>
{
    auto format(const anzu::node_type& type, auto& ctx) {
        return std::formatter<std::string>::format(anzu::to_string(type), ctx);
    }
};