#include "functions.hpp"
#include "object.hpp"
#include "runtime.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <unordered_map>
#include <string>
#include <functional>

namespace anzu {
namespace {

auto builtin_sqrt(std::span<const block> args) -> block
{
    const auto& val = std::get<block_float>(args[0]);
    return block_float{std::sqrt(val)};
}

auto builtin_print(std::span<const block> args) -> block
{
    const auto to_string = [](const block& blk) {
        return std::visit(overloaded{
            [](const block_str& blk) {
                return std::format("{}", format_special_chars(blk));
            },
            [&](const auto&) {
                return std::format("{}", blk);
            }
        }, blk);
    };

    if (args.size() > 1) {
        auto out = std::format("{{{}", to_string(args.front()));
        for (const auto& arg : args | std::views::drop(1)) {
            out += std::format(", {}", to_string(arg));
        }
        print("{}}}", out);
    } else {
        print("{}", to_string(args[0]));
    }
    return block{block_null{}};
}

auto builtin_println(std::span<const block> args) -> block
{
    const auto to_string = [](const block& blk) {
        return std::visit(overloaded{
            [](const block_str& blk) {
                return std::format("{}", format_special_chars(blk));
            },
            [&](const auto&) {
                return std::format("{}", blk);
            }
        }, blk);
    };

    if (args.size() == 0) {
        print("Empty println call!\n");
    }
    else if (args.size() > 1) {
        auto out = std::format("{{{}", to_string(args.front()));
        for (const auto& arg : args | std::views::drop(1)) {
            out += std::format(", {}", to_string(arg));
        }
        print("{}}}\n", out);
    } else {
        print("{}\n", to_string(args[0]));
    }
    return block{block_null{}};
}

auto builtin_put(std::span<const block> args) -> block
{
    anzu::print("{}", static_cast<char>(std::get<block_byte>(args[0])));
    return block{block_null{}};
}

auto builtin_input(std::span<const block> args) -> block
{
    std::string in;
    std::cin >> in;
    return block{in};
}

}

auto construct_builtin_map() -> std::unordered_map<std::string, builtin>
{
    auto builtins = std::unordered_map<std::string, builtin>{};

    builtins.emplace("sqrt", builtin{
        .ptr = builtin_sqrt,
        .sig = {
            .args = {
                { .name = "val", .type = float_type() }
            },
            .return_type = float_type()
        }
    });

    builtins.emplace("print", builtin{
        .ptr = builtin_print,
        .sig = {
            .args = {
                { .name = "obj", .type = int_type() }
            },
            .return_type = null_type()
        }
    });

    builtins.emplace("println", builtin{
        .ptr = builtin_println,
        .sig = {
            .args = {
                { .name = "obj", .type = int_type() }
            },
            .return_type = null_type()
        }
    });

    builtins.emplace("put", builtin{
        .ptr = builtin_put,
        .sig = {
            .args = {
                { .name = "c", .type = char_type() }
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