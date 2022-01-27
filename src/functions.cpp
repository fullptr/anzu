#include "functions.hpp"
#include "print.hpp"
#include "object.hpp"

#include <unordered_map>
#include <string>
#include <functional>

namespace anzu {
namespace {

auto push_null(anzu::context& ctx) -> void
{
    ctx.top().push(anzu::null_object());
}

auto verify(bool condition, std::string_view msg) -> void
{
    if (!condition) {
        anzu::print(msg);
        std::exit(1);
    }
}

auto builtin_print_frame(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    frame.print();
    push_null(ctx);
}

auto builtin_list_push(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    verify(frame.stack_size() >= 2, "stack must contain two elements for list_push\n");
    verify(frame.top(1).is<object_list>(), "second element on stack must be a list for list_push\n");
    auto elem = frame.pop();
    auto list = frame.pop();
    list.as<object_list>()->push_back(elem);
    push_null(ctx);
}

auto builtin_list_pop(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    verify(frame.top().is<object_list>(), "top element on stack must be a list for list_pop\n");
    auto list = frame.pop();
    frame.push(list.as<object_list>()->back());
    list.as<object_list>()->pop_back();
}

auto builtin_list_size(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    verify(frame.top().is<object_list>(), "top element on stack must be a list for list_size\n");
    auto list = frame.pop();
    frame.push(static_cast<int>(list.as<object_list>()->size()));
}

auto builtin_list_at(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    verify(frame.stack_size() >= 2, "stack must contain two elements for list_push\n");
    verify(frame.top(0).is<int>(), "first element of stack must be an integer (index into list)\n");
    verify(frame.top(1).is<object_list>(), "second element on stack must be a list for list_push\n");
    auto pos = frame.pop();
    auto list = frame.pop();
    frame.push(list.as<object_list>()->at(static_cast<std::size_t>(pos.to_int())));
}

auto builtin_to_int(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    frame.push(frame.pop().to_int());
}

auto builtin_to_bool(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    frame.push(frame.pop().to_bool());
}

auto builtin_to_str(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    frame.push(frame.pop().to_str());
}

auto builtin_print(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    const auto obj = frame.pop();
    if (obj.is<std::string>()) {
        anzu::print("{}", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}", obj);
    }
    push_null(ctx);
}

auto builtin_println(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    const auto obj = frame.pop();
    if (obj.is<std::string>()) {
        anzu::print("{}\n", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}\n", obj);
    }
    push_null(ctx);
}

auto builtin_input(anzu::context& ctx) -> void
{
    auto& frame = ctx.top();
    std::string in;
    std::cin >> in;
    frame.push(in);
}

}

static const std::unordered_map<std::string, builtin> builtins = {

    // List functions
    { "list_push",       builtin{ builtin_list_push,   2 }},
    { "list_pop",        builtin{ builtin_list_pop,    1 }},
    { "list_size",       builtin{ builtin_list_size,   1 }},
    { "list_at",         builtin{ builtin_list_at,     2 }},

    // Debug functions
    { "__print_frame__", builtin{ builtin_print_frame, 0 }},

    // Old Op Codes
    { "to_int",          builtin{ builtin_to_int,      1 }},
    { "to_bool",         builtin{ builtin_to_bool,     1 }},
    { "to_str",          builtin{ builtin_to_str,      1 }},

    // I/O
    { "print",           builtin{ builtin_print,       1 }},
    { "println",         builtin{ builtin_println,     1 }},
    { "input",           builtin{ builtin_input,       0 }}
};

auto is_builtin(const std::string& name) -> bool
{
    return builtins.contains(name);
}

auto fetch_builtin(const std::string& name) -> builtin_function
{
    return builtins.at(name).ptr;
}

auto fetch_builtin_argc(const std::string& name) -> std::int64_t
{
    return builtins.at(name).argc;
}

}