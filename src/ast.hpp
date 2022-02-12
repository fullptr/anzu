#pragma once
#include "object.hpp"
#include "functions.hpp"
#include "token.hpp"

#include <variant>
#include <vector>
#include <memory>

namespace anzu {

struct node_expr;
using node_expr_ptr = std::unique_ptr<node_expr>;

struct node_literal_expr
{
    anzu::object value;
};

struct node_variable_expr
{
    std::string name;
};

struct node_bin_op_expr
{
    anzu::token   op; // Keep as token for debugging info
    node_expr_ptr lhs;
    node_expr_ptr rhs;
};

struct node_function_call_expr
{
    std::string                function_name;
    std::vector<node_expr_ptr> args;
};

struct node_expr : std::variant<
    node_literal_expr,
    node_variable_expr,
    node_bin_op_expr,
    node_function_call_expr>
{
};

struct node_stmt;
using node_stmt_ptr = std::unique_ptr<node_stmt>;

struct node_sequence_stmt
{
    std::vector<node_stmt_ptr> sequence;
};

struct node_while_stmt
{
    node_expr_ptr condition;
    node_stmt_ptr body;
};

struct node_if_stmt
{
    node_expr_ptr condition;
    node_stmt_ptr body;
    node_stmt_ptr else_body;
};

struct node_for_stmt
{
    std::string   var;
    node_expr_ptr container;
    node_stmt_ptr body;
};

struct node_break_stmt
{
};

struct node_continue_stmt
{
};

struct node_assignment_stmt
{
    std::string   name;
    node_expr_ptr expr;
};

struct node_function_def_stmt
{
    std::string        name;
    function_signature sig;
    node_stmt_ptr      body;
};

struct node_function_call_stmt
{
    std::string                function_name;
    std::vector<node_expr_ptr> args;
};

struct node_return_stmt
{
    node_expr_ptr return_value;
};

struct node_debug_stmt
{
};

struct node_stmt : std::variant<
    node_sequence_stmt,
    node_while_stmt,
    node_if_stmt,
    node_for_stmt,
    node_break_stmt,
    node_continue_stmt,
    node_assignment_stmt,
    node_function_def_stmt,
    node_function_call_stmt,
    node_return_stmt,
    node_debug_stmt>
{
};

auto print_node(const anzu::node_expr& node, int indent = 0) -> void;
auto print_node(const anzu::node_stmt& node, int indent = 0) -> void;

}