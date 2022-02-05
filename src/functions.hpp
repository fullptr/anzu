#pragma once
#include <string>
#include <vector>

namespace anzu {

struct function_signature
{
    struct arg
    {
        std::string name;
        std::string type = "any";
    };

    std::vector<arg> args;
    std::string return_type = "any";
};

class runtime_context;
using builtin_function = void(*)(anzu::runtime_context&);

struct builtin
{
    builtin_function ptr;
    std::int64_t     argc;
};

auto is_builtin(const std::string& name) -> bool;
auto fetch_builtin(const std::string& name) -> builtin_function;
auto fetch_builtin_argc(const std::string& name) -> std::int64_t;
    
}