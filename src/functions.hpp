#pragma once
#include "object.hpp"

#include <functional>
#include <string>
#include <vector>
#include <span>
#include <optional>

namespace anzu {

struct runtime_context;
using builtin_function = std::function<void(runtime_context&)>;

struct builtin
{
    std::string            name;
    builtin_function       ptr;
    std::vector<type_name> args;
    type_name              return_type;
};

auto get_builtin_id(const std::string& name, const std::vector<type_name>& args)
    -> std::optional<std::size_t>;

auto get_builtin_return_type(std::size_t id) -> type_name;
auto get_builtin_function_ptr(std::size_t id) -> builtin_function;

}