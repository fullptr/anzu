#pragma once
#include "type.hpp"

#include <string>
#include <vector>
#include <span>

namespace anzu {

class block;
using builtin_function = block(*)(std::span<const block>);

struct builtin
{
    builtin_function ptr;
    signature        sig;
};

auto is_builtin(const std::string& name) -> bool;
auto fetch_builtin(const std::string& name) -> const builtin&;
    
}