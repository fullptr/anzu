#pragma once
#include "ast.hpp"
#include "object.hpp"
#include "type.hpp"

#include <string>
#include <unordered_map>

namespace anzu {

using expr_types = std::unordered_map<const node_expr*, type>;

// Scans the AST and performs the following:
//      - evaluates the type of all expressions to verify they are valid
//      - verify that expressions passed as function arguments match the function signatures
//      - verify that the types listed in function signatures are valid types
auto typecheck_ast(const node_stmt_ptr& ast) -> expr_types;

}