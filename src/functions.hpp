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
auto to_string(const function_signature& sig) -> std::string;

inline auto operator==(const function_signature::arg& lhs, const function_signature::arg& rhs) -> bool
{
    return std::tie(lhs.name, lhs.type) == std::tie(rhs.name, rhs.type);
}

inline auto operator==(const function_signature& lhs, const function_signature& rhs) -> bool
{
    return std::tie(lhs.args, lhs.return_type) == std::tie(rhs.args, rhs.return_type);
}

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