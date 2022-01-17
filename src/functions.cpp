#include "functions.hpp"

#include <unordered_map>
#include <string>
#include <functional>

static const std::unordered_map<std::string, std::function<void(anzu::context&)>> builtins = {

};

namespace anzu {

auto is_builtin(const std::string& name) -> bool
{
    return builtins.contains(name);
}

auto call_builtin(const std::string& name, anzu::context& ctx) -> void
{
    builtins.at(name)(ctx);
}

}