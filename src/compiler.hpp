#pragma once
#include "ast.hpp"
#include "parser.hpp"
#include "bytecode.hpp"

#include "compilation/type_manager.hpp"
#include "compilation/variable_manager.hpp"

#include <filesystem>
#include <map>
#include <string>
#include <vector>
#include <unordered_map>

namespace anzu {

struct signature
{
    std::vector<type_name> params;
    type_name              return_type;
};

using template_map = std::unordered_map<type_name, type_name, type_hash>;

struct function_info
{
    std::string      name;
    signature        sig;
    token            tok;
    variable_manager variables;
    
    std::size_t            id;
    std::vector<std::byte> code;

    template_map map = {};
};

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    std::vector<std::size_t> current_compiling;
    std::unordered_map<std::string, node_function_def_stmt> function_templates;

    std::string rom;

    std::unordered_map<std::string, std::size_t> functions_by_name;
    std::vector<function_info>                   functions;
    
    type_manager     types;
};

auto compile(const anzu_module& ast) -> bytecode_program;

}