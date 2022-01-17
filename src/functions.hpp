#pragma once
// Builtin functions that can be called from scripts
#include "stack_frame.hpp"

#include <string>

namespace anzu {

auto is_builtin(const std::string& name) -> bool;
auto call_builtin(const std::string& name, anzu::context& ctx) -> void;
    
}