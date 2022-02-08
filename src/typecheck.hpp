#pragma once
#include "ast.hpp"
#include "functions.hpp"

namespace anzu {

struct parser_context;

// Given a context and function name along with a set of arguments, verify that the
// arguments match the function signature.
auto type_check_function_call(
    const parser_context& ctx,
    const std::string& function_name,
    std::span<const node_expr_ptr> args
) -> void;

// Evaluates a given expression node with the given context. Produces an error if the
// expression is invalid, otherwise the returns the type of the expression.
auto type_of_expr(const parser_context& ctx, const node_expr& node) -> std::string;

}