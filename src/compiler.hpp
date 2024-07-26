#pragma once
#include "ast.hpp"
#include "parser.hpp"
#include "bytecode.hpp"

#include "compilation/type_manager.hpp"
#include "compilation/variable_manager.hpp"

#include <filesystem>
#include <map>
#include <string>
#include <stack>
#include <unordered_map>

namespace anzu {

struct signature
{
    std::vector<type_name> params;
    type_name              return_type;
};

struct function_info
{
    std::string            name;
    signature              sig;
    token                  tok;
    
    std::size_t            id;
    std::vector<std::byte> code;
};

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    // Returns the current function
    auto current() -> function_info& {
        return functions[current_compiling.top()];
    }

    // Returns the bytecode that we are currently writing to
    auto code() -> std::vector<std::byte>& {
        return current().code;
    }

    auto in_function() const -> bool {
        return current_compiling.size() > 1;
    }

    std::stack<std::size_t> current_compiling;
    std::unordered_map<std::string, node_function_def_stmt> function_templates;

    std::string rom;

    std::unordered_map<std::string, std::size_t> functions_by_name;
    std::vector<function_info>                   functions;
    
    type_manager     types;
    variable_manager variables;
};

auto compile(const anzu_module& ast) -> bytecode_program;

}