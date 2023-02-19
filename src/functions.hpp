#pragma once
#include "object.hpp"

#include <functional>
#include <string>
#include <vector>
#include <span>

namespace anzu {

struct runtime_context;
using builtin_function = std::function<void(runtime_context&)>;

struct builtin_key
{
    std::string            name;
    std::vector<type_name> args;
    auto operator==(const builtin_key&) const -> bool = default;
};

inline auto hash(const builtin_key& f) -> std::size_t
{
    auto hash_value = std::hash<std::string>{}(f.name);
    for (const auto& arg : f.args) {
        hash_value ^= hash(arg);
    }
    return hash_value;
}

struct builtin_val
{
    builtin_function ptr;
    type_name        return_type;
};

using builtin_hash = decltype([](const builtin_key& x) { return hash(x); });
using builtin_map = std::unordered_map<builtin_key, builtin_val, builtin_hash>;

auto is_builtin(const std::string& name, const std::vector<type_name>& args) -> bool;
auto fetch_builtin(const std::string& name, const std::vector<type_name>& args) -> builtin_val;
    
}