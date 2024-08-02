#pragma once
#include "ast.hpp"
#include "token.hpp"
#include "lexer.hpp"

namespace anzu {

auto parse_expr(tokenstream& tokens) -> node_expr_ptr;

}