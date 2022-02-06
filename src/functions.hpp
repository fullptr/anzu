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

class object;
using builtin_function = object(*)(const std::vector<object>&);

struct builtin
{
    builtin_function   ptr;
    function_signature sig;
};

auto is_builtin(const std::string& name) -> bool;
auto fetch_builtin(const std::string& name) -> const builtin&;
    
}