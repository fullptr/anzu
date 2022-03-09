#pragma once
#include "type.hpp"

#include <string>
#include <vector>
#include <span>

namespace anzu {

struct block;
using builtin_function = block(*)(std::span<const block>);

// A more dangerous function pointer type that had access to the entire memory
// vector, and should only be allowed for internal implementations of builtin types.
using builtin_mem_op = void(*)(std::vector<block>& memory);

struct builtin
{
    builtin_function ptr;
    signature        sig;
};

auto is_builtin(const std::string& name) -> bool;
auto fetch_builtin(const std::string& name) -> const builtin&;
    
}