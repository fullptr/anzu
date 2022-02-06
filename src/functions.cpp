#include "functions.hpp"
#include "object.hpp"
#include "runtime.hpp"
#include "utility/print.hpp"

#include <unordered_map>
#include <string>
#include <functional>

namespace anzu {
namespace {

auto push_null(anzu::runtime_context& ctx) -> void
{
    ctx.push_value(anzu::null_object());
}

auto verify(bool condition, std::string_view msg) -> void
{
    if (!condition) {
        anzu::print(msg);
        std::exit(1);
    }
}

auto builtin_print_frame(anzu::runtime_context& ctx) -> void
{
    ctx.peek_frame().memory.print();
    push_null(ctx);
}

auto builtin_list_push(anzu::runtime_context& ctx) -> void
{
    verify(ctx.size() >= 2, "stack must contain two elements for list_push\n");
    verify(ctx.peek_value(1).is<object_list>(), "second element on stack must be a list for list_push\n");
    auto elem = ctx.pop_value();
    auto list = ctx.pop_value();
    list.as<object_list>()->push_back(elem);
    push_null(ctx);
}

auto builtin_list_pop(anzu::runtime_context& ctx) -> void
{
    verify(ctx.peek_value().is<object_list>(), "top element on stack must be a list for list_pop\n");
    auto list = ctx.pop_value();
    ctx.push_value(list.as<object_list>()->back());
    list.as<object_list>()->pop_back();
}

auto builtin_list_size(anzu::runtime_context& ctx) -> void
{
    verify(ctx.peek_value().is<object_list>(), "top element on stack must be a list for list_size\n");
    auto list = ctx.pop_value();
    ctx.push_value(static_cast<int>(list.as<object_list>()->size()));
}

auto builtin_list_at(anzu::runtime_context& ctx) -> void
{
    verify(ctx.size() >= 2, "stack must contain two elements for list_push\n");
    verify(ctx.peek_value(0).is<int>(), "first element of stack must be an integer (index into list)\n");
    verify(ctx.peek_value(1).is<object_list>(), "second element on stack must be a list for list_push\n");
    auto pos = ctx.pop_value();
    auto list = ctx.pop_value();
    ctx.push_value(list.as<object_list>()->at(static_cast<std::size_t>(pos.to_int())));
}

auto builtin_to_int(anzu::runtime_context& ctx) -> void
{
    ctx.push_value(ctx.pop_value().to_int());
}

auto builtin_to_bool(anzu::runtime_context& ctx) -> void
{
    ctx.push_value(ctx.pop_value().to_bool());
}

auto builtin_to_str(anzu::runtime_context& ctx) -> void
{
    ctx.push_value(ctx.pop_value().to_str());
}

auto builtin_print(anzu::runtime_context& ctx) -> void
{
    const auto obj = ctx.peek_value();
    if (obj.is<std::string>()) {
        anzu::print("{}", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}", obj);
    }
}

auto builtin_println(anzu::runtime_context& ctx) -> void
{
    const auto obj = ctx.peek_value();
    if (obj.is<std::string>()) {
        anzu::print("{}\n", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}\n", obj);
    }
}

auto builtin_input(anzu::runtime_context& ctx) -> void
{
    std::string in;
    std::cin >> in;
    ctx.push_value(in);
}

}

auto construct_builtin_map() -> std::unordered_map<std::string, builtin>
{
    auto builtins = std::unordered_map<std::string, builtin>{};

    builtins.emplace("list_push", builtin{
        .ptr = builtin_list_push,
        .sig = {
            .args = {
                { .name = "list_obj", .type = "list" },
                { .name = "value",    .type = "any"  }
            },
            .return_type = "null"
        }
    });

    builtins.emplace("list_pop", builtin{
        .ptr = builtin_list_pop,
        .sig = {
            .args = {
                { .name = "list_obj", .type = "list" }
            },
            .return_type = "any"
        }
    });

    builtins.emplace("list_size", builtin{
        .ptr = builtin_list_size,
        .sig = {
            .args = {
                { .name = "list_obj", .type = "list" }
            },
            .return_type = "int"
        }
    });

    builtins.emplace("list_at", builtin{
        .ptr = builtin_list_at,
        .sig = {
            .args = {
                { .name = "list_obj", .type = "list" },
                { .name = "index",    .type = "int"  }
            },
            .return_type = "any"
        }
    });

    builtins.emplace("__print_frame__", builtin{
        .ptr = builtin_print_frame,
        .sig = {
            .args = {},
            .return_type = "null"
        }
    });

    builtins.emplace("to_int", builtin{
        .ptr = builtin_to_int,
        .sig = {
            .args = {
                { .name = "obj", .type = "any" }
            },
            .return_type = "int"
        }
    });

    builtins.emplace("to_bool", builtin{
        .ptr = builtin_to_bool,
        .sig = {
            .args = {
                { .name = "obj", .type = "any" }
            },
            .return_type = "bool"
        }
    });

    builtins.emplace("to_str", builtin{
        .ptr = builtin_to_int,
        .sig = {
            .args = {
                { .name = "obj", .type = "any" }
            },
            .return_type = "str"
        }
    });

    builtins.emplace("print", builtin{
        .ptr = builtin_print,
        .sig = {
            .args = {
                { .name = "obj", .type = "any" }
            },
            .return_type = "null"
        }
    });

    builtins.emplace("println", builtin{
        .ptr = builtin_println,
        .sig = {
            .args = {
                { .name = "obj", .type = "any" }
            },
            .return_type = "null"
        }
    });

    builtins.emplace("input", builtin{
        .ptr = builtin_println,
        .sig = {
            .args = {},
            .return_type = "str"
        }
    });

    return builtins;
}

static const std::unordered_map<std::string, builtin> builtins = construct_builtin_map();

auto is_builtin(const std::string& name) -> bool
{
    return builtins.contains(name);
}

auto fetch_builtin(const std::string& name) -> builtin_function
{
    auto it = builtins.find(name);
    if (it == builtins.end()) {
        anzu::print("builtin error: could not find function '{}'\n", name);
        std::exit(1);
    }
    return it->second.ptr;
}

auto fetch_builtin_argc(const std::string& name) -> std::int64_t
{
    auto it = builtins.find(name);
    if (it == builtins.end()) {
        anzu::print("builtin error: could not find function '{}'\n", name);
        std::exit(1);
    }
    return it->second.sig.args.size();
}

}