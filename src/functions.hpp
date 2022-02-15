#pragma once
#include "type.hpp"

#include <string>
#include <vector>
#include <span>

namespace anzu {

struct function_signature
{
    struct arg
    {
        std::string name;
        anzu::type  type = make_generic(0);
    };

    std::vector<arg> args;
    anzu::type       return_type = make_generic(0);
};

class object;
using builtin_function = object(*)(std::span<const object>);

struct builtin
{
    builtin_function   ptr;
    function_signature sig;
};

auto is_builtin(const std::string& name) -> bool;
auto fetch_builtin(const std::string& name) -> const builtin&;
    
}