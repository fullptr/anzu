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
    if (args.size() > 1) {
        auto out = std::format("{{{}", to_string(args.front()));
        for (const auto& arg : args | std::views::drop(1)) {
            out += std::format(", {}", arg);
        }
        print("{}}}", out);
    } else {
        print("{}", to_string(args[0]));
    }
    return block{block_null{}};
}

auto builtin_println(std::span<const block> args) -> block
{
    if (args.size() == 0) {
        print("Empty println call!\n");
    }
    else if (args.size() > 1) {
        auto out = std::format("{{{}", to_string(args.front()));
        for (const auto& arg : args | std::views::drop(1)) {
            out += std::format(", {}", arg);
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

}

auto construct_builtin_map() -> builtin_map
{
    auto builtins = builtin_map{};

    builtins.emplace(
        builtin_key{ .name = "sqrt", .args = { float_type() } },
        builtin_val{ .ptr = builtin_sqrt, .return_type = float_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { int_type() } },
        builtin_val{ .ptr = builtin_print, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "println", .args = { int_type() } },
        builtin_val{ .ptr = builtin_println, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "put", .args = { char_type() } },
        builtin_val{ .ptr = builtin_put, .return_type = null_type() }
    );

    return builtins;
}

static const auto builtins = construct_builtin_map();

auto is_builtin(const std::string& name, const std::vector<type_name>& args) -> bool
{
    return builtins.contains({name, args});
}

auto fetch_builtin(const std::string& name, const std::vector<type_name>& args) -> const builtin_val&
{
    auto it = builtins.find({name, args});
    if (it == builtins.end()) {
        anzu::print("builtin error: could not find function '{}'\n", name);
        std::exit(1);
    }
    return it->second;
}

}