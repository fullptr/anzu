#include "functions.hpp"
#include "print.hpp"

#include <unordered_map>
#include <string>
#include <functional>

namespace {

auto verify(bool condition, std::string_view msg) -> void
{
    if (!condition) {
        anzu::print(msg);
        std::exit(1);
    }
}

auto builtin_print(anzu::context& ctx) -> void
{
    verify(!ctx.top().empty(), "frame stack is empty, nothing to print\n");
    anzu::print("{}", ctx.top().pop());
    ctx.top().ptr() += 1;
}

}

static const std::unordered_map<std::string, std::function<void(anzu::context&)>> builtins = {
    { "print", builtin_print }
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