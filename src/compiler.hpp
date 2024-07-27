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

struct function_info
{
    std::string      name;
    std::size_t      id;
    variable_manager variables;
    template_map     template_types;
    
    std::vector<std::byte> code = {};
    signature              sig  = {};
};

struct compiler
{
    std::vector<function_info> functions;
    std::string                rom;

    type_manager types;

    std::unordered_map<std::string, std::size_t>            functions_by_name;
    std::unordered_map<std::string, node_function_def_stmt> function_templates;
    std::vector<std::size_t>                                current_compiling;
};

auto compile(const anzu_module& ast) -> bytecode_program;

}