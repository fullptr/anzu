#pragma once
#include "ast.hpp"
#include "functions.hpp"

namespace anzu {

struct parser_context;

auto fetch_function_signature(
    const parser_context& ctx, const std::string& function_name
) -> function_signature;

// Evaluates a given expression node with the given context. Produces an error if the
// expression is invalid, otherwise the returns the type of the expression.
auto type_of_expr(const parser_context& ctx, const node_expr& node) -> std::string;

}