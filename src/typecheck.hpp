#pragma once
#include "ast.hpp"
#include "object.hpp"
#include "type.hpp"

#include <string>
#include <unordered_map>

namespace anzu {

struct type_info
{
    type_store types;
};

// Scans the AST and performs the following:
//      - evaluates the type of all expressions to verify they are valid
//      - verify that expressions passed as function arguments match the function signatures
//      - verify that the types listed in function signatures are valid types
void typecheck_ast(const node_stmt_ptr& ast);

}