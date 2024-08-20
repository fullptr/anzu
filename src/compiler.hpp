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
#include <unordered_set>

namespace anzu {

struct signature
{
    std::vector<type_name> params;
    type_name              return_type;
};

struct function
{
    std::string      name;
    std::size_t      id;
    variable_manager variables;
    
    std::vector<std::byte> code = {};
    signature              sig  = {};
};

struct func_info
{
    std::size_t  id;
    template_map templates;
};

struct struct_info
{
    type_name    name;
    template_map templates;
};

using module_map = std::unordered_map<std::string, std::filesystem::path>;
struct module_info
{
    std::filesystem::path filepath;
    module_map            imports;
};

struct template_struct_type
{
    std::string name;
    std::filesystem::path module;
    auto operator==(const template_struct_type&) const -> bool = default;
};
using template_struct_type_hash = decltype([](const template_struct_type& x) {
    return std::hash<std::string>{}(x.name) ^ std::hash<std::string>{}(x.module.string());
});

struct template_function_type
{
    std::string name;
    std::filesystem::path module;
    type_name struct_name;
    auto operator==(const template_function_type&) const -> bool = default;
};
using template_function_type_hash = decltype([](const template_function_type& x) {
    return std::hash<std::string>{}(x.name) ^ std::hash<std::string>{}(x.module.string())
        ^ anzu::hash(x.struct_name);
});

struct function_key
{
    type_struct struct_type;
};

struct compiler
{
    std::vector<function> functions;
    std::string           rom;

    type_manager types;

    std::unordered_set<std::filesystem::path> modules;

    std::unordered_map<std::string, std::size_t> functions_by_name;
    
    std::unordered_map<template_function_type, node_function_stmt, template_function_type_hash> function_templates;
    std::unordered_map<template_struct_type, node_struct_stmt, template_struct_type_hash> struct_templates;

    std::vector<struct_info> current_struct;
    std::vector<func_info>   current_function;
    std::vector<module_info> current_module;
};

auto compile(const anzu_module& ast) -> bytecode_program;

}