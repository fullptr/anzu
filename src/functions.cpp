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

auto builtin_list_push(std::span<const object> args) -> object
{
    auto& list = args[0].as<object_list>();
    auto& obj = args[1];
    list->push_back(obj);
    return null_object();
}

auto builtin_list_pop(std::span<const object> args) -> object
{
    auto& list = args[0].as<object_list>();
    auto ret = list->back();
    list->pop_back();
    return ret;
}

auto builtin_list_size(std::span<const object> args) -> object
{
    const auto& list = args[0].as<object_list>();
    return static_cast<int>(list->size());
}

auto builtin_list_at(std::span<const object> args) -> object
{
    const auto& list = args[0].as<object_list>();
    const auto& idx = args[1].as<int>();
    return list->at(idx);
}

auto builtin_to_int(std::span<const object> args) -> object
{
    return args[0].to_int();
}

auto builtin_to_bool(std::span<const object> args) -> object
{
    return args[0].to_bool();
}

auto builtin_to_str(std::span<const object> args) -> object
{
    return args[0].to_str();
}

auto builtin_print(std::span<const object> args) -> object
{
    const auto& obj = args[0];
    if (obj.is<std::string>()) {
        anzu::print("{}", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}", obj);
    }
    return null_object();
}

auto builtin_println(std::span<const object> args) -> object
{
    const auto& obj = args[0];
    if (obj.is<std::string>()) {
        anzu::print("{}\n", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}\n", obj);
    }
    return null_object();
}

auto builtin_input(std::span<const object> args) -> object
{
    std::string in;
    std::cin >> in;
    return in;
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

auto fetch_builtin(const std::string& name) -> const builtin&
{
    auto it = builtins.find(name);
    if (it == builtins.end()) {
        anzu::print("builtin error: could not find function '{}'\n", name);
        std::exit(1);
    }
    return it->second;
}

}