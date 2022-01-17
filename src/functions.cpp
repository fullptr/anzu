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

auto builtin_stack_size(anzu::context& ctx) -> void
{
    auto stack_size = static_cast<int>(ctx.top().stack_size());
    ctx.top().push(stack_size);
    ctx.top().ptr() += 1;
};

auto builtin_print_frame(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    frame.print();
    frame.ptr() += 1;
}

auto builtin_list_new(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    frame.push(std::make_shared<std::vector<anzu::object>>());
    frame.ptr() += 1;
}

auto builtin_list_push(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    verify(frame.stack_size() >= 2, "stack must contain two elements for list_push\n");
    verify(frame.top(1).is_list(), "second element on stack must be a list for list_push\n");
    auto elem = frame.pop();
    frame.top().as_list()->push_back(elem);
    frame.ptr() += 1;
}

}

static const std::unordered_map<std::string, std::function<void(anzu::context&)>> builtins = {
    { "print", builtin_print },
    { "stack_size", builtin_stack_size },

    // List functions
    { "list_new", builtin_list_new },
    { "list_push", builtin_list_push },

    // Debug functions
    { "__print_frame__", builtin_print_frame }
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