#include "functions.hpp"
#include "object.hpp"
#include "runtime.hpp"
#include "typecheck.hpp"
#include "utility/print.hpp"

#include <unordered_map>
#include <string>
#include <functional>

namespace anzu {
namespace {

auto builtin_list_push(std::span<const block> args) -> block
{
    auto& list = args[0].as<block_list>();
    auto& obj = args[1];
    list->push_back(obj);
    return block{block_null{}};
}

auto builtin_list_pop(std::span<const block> args) -> block
{
    auto& list = args[0].as<block_list>();
    auto ret = list->back();
    list->pop_back();
    return ret;
}

auto builtin_list_size(std::span<const block> args) -> block
{
    const auto& list = args[0].as<block_list>();
    return block{static_cast<int>(list->size())};
}

auto builtin_list_at(std::span<const block> args) -> block
{
    const auto& list = args[0].as<block_list>();
    const auto& idx = args[1].as<int>();
    return list->at(idx);
}

auto builtin_str_size(std::span<const block> args) -> block
{
    const auto& str = args[0].as<std::string>();
    return block{static_cast<int>(str.size())};
}

auto builtin_str_at(std::span<const block> args) -> block
{
    const auto& str = args[0].as<std::string>();
    const auto& idx = args[1].as<int>();
    return block{block_str{str.at(idx)}};
}

auto builtin_print(std::span<const block> args) -> block
{
    const auto& obj = args[0];
    if (obj.is<std::string>()) {
        anzu::print("{}", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}", obj);
    }
    return block{block_null{}};
}

auto builtin_println(std::span<const block> args) -> block
{
    const auto& obj = args[0];
    if (obj.is<std::string>()) {
        anzu::print("{}\n", anzu::format_special_chars(obj.as<std::string>()));
    } else {
        anzu::print("{}\n", obj);
    }
    return block{block_null{}};
}

auto builtin_input(std::span<const block> args) -> block
{
    std::string in;
    std::cin >> in;
    return block{in};
}

auto builtin_range(std::span<const block> args) -> block
{
    const auto& max = args[0].as<int>();
    auto list = std::make_shared<std::vector<block>>();
    for (int i = 0; i != max; ++i) {
        list->push_back(block{i});
    }
    return block{list};
}

}

auto construct_builtin_map() -> std::unordered_map<std::string, builtin>
{
    auto builtins = std::unordered_map<std::string, builtin>{};

    builtins.emplace("list_push", builtin{
        .ptr = builtin_list_push,
        .sig = {
            .args = {
                { .name = "list_obj", .type = generic_list_type() },
                { .name = "value",    .type = generic_type(0)  }
            },
            .return_type = null_type()
        }
    });

    builtins.emplace("list_pop", builtin{
        .ptr = builtin_list_pop,
        .sig = {
            .args = {
                { .name = "list_obj", .type = generic_list_type() }
            },
            .return_type = generic_type(0)
        }
    });

    builtins.emplace("list_size", builtin{
        .ptr = builtin_list_size,
        .sig = {
            .args = {
                { .name = "list_obj", .type = generic_list_type() }
            },
            .return_type = int_type()
        }
    });

    builtins.emplace("list_at", builtin{
        .ptr = builtin_list_at,
        .sig = {
            .args = {
                { .name = "list_obj", .type = generic_list_type() },
                { .name = "index",    .type = int_type() }
            },
            .return_type = generic_type(0)
        }
    });

    builtins.emplace("str_size", builtin{
        .ptr = builtin_str_size,
        .sig = {
            .args = {
                { .name = "string", .type = str_type() }
            },
            .return_type = int_type()
        }
    });

    builtins.emplace("str_at", builtin{
        .ptr = builtin_str_at,
        .sig = {
            .args = {
                { .name = "string", .type = str_type() },
                { .name = "index",    .type = int_type() }
            },
            .return_type = str_type()
        }
    });

    builtins.emplace("print", builtin{
        .ptr = builtin_print,
        .sig = {
            .args = {
                { .name = "obj", .type = generic_type(0) }
            },
            .return_type = null_type()
        }
    });

    builtins.emplace("println", builtin{
        .ptr = builtin_println,
        .sig = {
            .args = {
                { .name = "obj", .type = generic_type(0) }
            },
            .return_type = null_type()
        }
    });

    builtins.emplace("input", builtin{
        .ptr = builtin_input,
        .sig = {
            .args = {},
            .return_type = str_type()
        }
    });

    builtins.emplace("range", builtin{
        .ptr = builtin_range,
        .sig = {
            .args = {
                { .name = "max", .type = int_type() }
            },
            .return_type = concrete_list_type(int_type())
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