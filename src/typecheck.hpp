#pragma once
#include "ast.hpp"
#include "object.hpp"
#include "type.hpp"

#include <string>

namespace anzu {

// Scans the AST and performs the following:
//      - evaluates the type of all expressions to verify they are valid
//      - verify that expressions passed as function arguments match the function signatures
//      - verify that the types listed in function signatures are valid types
auto typecheck_ast(const node_stmt_ptr& ast) -> void;

}