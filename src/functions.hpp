#pragma once
// Builtin functions that can be called from scripts
#include "stack_frame.hpp"

#include <string>

namespace anzu {

using builtin_function = void(*)(anzu::context&);

auto is_builtin(const std::string& name) -> bool;
auto fetch_builtin(const std::string& name) -> builtin_function;
    
}