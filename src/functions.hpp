#pragma once
// Builtin functions that can be called from scripts

#include <string>

namespace anzu {

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