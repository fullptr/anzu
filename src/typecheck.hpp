#pragma once
#include "ast.hpp"
#include "object.hpp"
#include "type.hpp"

#include <string>

namespace anzu {

// This doesn't belong in typecheck.hpp. In the future when we create an actual struct
// for representing types, this can be moved into that module.
auto type_of(const anzu::object& object) -> type;

// Scans the AST and performs the following:
//      - evaluates the type of all expressions to verify they are valid
//      - verify that expressions passed as function arguments match the function signatures
//      - verify that the types listed in function signatures are valid types
auto typecheck_ast(const node_stmt_ptr& ast) -> void;

}