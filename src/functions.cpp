#include "functions.hpp"
#include "object.hpp"
#include "runtime.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <unordered_map>
#include <string>
#include <functional>
#include <utility>

namespace anzu {
namespace {

auto builtin_sqrt(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, sizeof(std::uint64_t)>();
    std::memcpy(bytes.data(), args.data(), sizeof(std::uint64_t));
    const auto val = std::sqrt(std::bit_cast<double>(bytes));
    return to_bytes(val);
}

auto builtin_print_char(std::span<const std::byte> args) -> std::vector<std::byte>
{
    print("{}", static_cast<char>(args[0]));
    return std::vector{std::byte{0}};
}

auto builtin_println_char(std::span<const std::byte> args) -> std::vector<std::byte>
{
    print("{}\n", static_cast<char>(args[0]));
    return std::vector{std::byte{0}};
}

auto builtin_print_bool(std::span<const std::byte> args) -> std::vector<std::byte>
{
    print("{}", args[0] == std::byte{1});
    return std::vector{std::byte{0}};
}

auto builtin_println_bool(std::span<const std::byte> args) -> std::vector<std::byte>
{
    print("{}\n", args[0] == std::byte{1});
    return std::vector{std::byte{0}};
}

auto builtin_print_null(std::span<const std::byte> args) -> std::vector<std::byte>
{
    print("null");
    return std::vector{std::byte{0}};
}

auto builtin_println_null(std::span<const std::byte> args) -> std::vector<std::byte>
{
    print("null\n");
    return std::vector{std::byte{0}};
}

auto builtin_print_i32(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 4>();
    for (std::size_t i = 0; i != 4; ++i) {
        bytes[i] = args[i];
    }
    print("{}", std::bit_cast<std::int32_t>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_println_i32(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 4>();
    for (std::size_t i = 0; i != 4; ++i) {
        bytes[i] = args[i];
    }
    print("{}\n", std::bit_cast<std::int32_t>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_print_i64(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 8>();
    for (std::size_t i = 0; i != 8; ++i) {
        bytes[i] = args[i];
    }
    print("{}", std::bit_cast<std::int64_t>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_println_i64(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 8>();
    for (std::size_t i = 0; i != 8; ++i) {
        bytes[i] = args[i];
    }
    print("{}\n", std::bit_cast<std::int64_t>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_print_u64(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 8>();
    for (std::size_t i = 0; i != 8; ++i) {
        bytes[i] = args[i];
    }
    print("{}", std::bit_cast<std::uint64_t>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_println_u64(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 8>();
    for (std::size_t i = 0; i != 8; ++i) {
        bytes[i] = args[i];
    }
    print("{}\n", std::bit_cast<std::uint64_t>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_print_f64(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 8>();
    for (std::size_t i = 0; i != 8; ++i) {
        bytes[i] = args[i];
    }
    print("{}", std::bit_cast<double>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_println_f64(std::span<const std::byte> args) -> std::vector<std::byte>
{
    auto bytes = std::array<std::byte, 8>();
    for (std::size_t i = 0; i != 8; ++i) {
        bytes[i] = args[i];
    }
    print("{}\n", std::bit_cast<double>(bytes));
    return std::vector{std::byte{0}};
}

auto builtin_put(std::span<const std::byte> args) -> std::vector<std::byte>
{
    anzu::print("{}", static_cast<char>(args[0]));
    return std::vector{std::byte{0}};
}

}

auto construct_builtin_map() -> builtin_map
{
    auto builtins = builtin_map{};

    builtins.emplace(
        builtin_key{ .name = "sqrt", .args = { f64_type() } },
        builtin_val{ .ptr = builtin_sqrt, .return_type = f64_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { u64_type() } },
        builtin_val{ .ptr = builtin_print_u64, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { u64_type() } },
        builtin_val{ .ptr = builtin_println_u64, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { char_type() } },
        builtin_val{ .ptr = builtin_print_char, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { char_type() } },
        builtin_val{ .ptr = builtin_println_char, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { f64_type() } },
        builtin_val{ .ptr = builtin_print_f64, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { f64_type() } },
        builtin_val{ .ptr = builtin_println_f64, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { bool_type() } },
        builtin_val{ .ptr = builtin_print_bool, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { bool_type() } },
        builtin_val{ .ptr = builtin_println_bool, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { null_type() } },
        builtin_val{ .ptr = builtin_print_null, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { null_type() } },
        builtin_val{ .ptr = builtin_println_null, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { i32_type() } },
        builtin_val{ .ptr = builtin_print_i32, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { i32_type() } },
        builtin_val{ .ptr = builtin_println_i32, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { i64_type() } },
        builtin_val{ .ptr = builtin_print_i64, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { i64_type() } },
        builtin_val{ .ptr = builtin_println_i64, .return_type = null_type() }
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
    // Hack, generalise later
    if (name.starts_with("print") &&
        args.size() == 1 &&
        std::holds_alternative<type_list>(args[0]) &&
        inner_type(args[0]) == char_type()
    ) {
        return true;
    }
    return builtins.contains({name, args});
}

auto fetch_builtin(const std::string& name, const std::vector<type_name>& args) -> builtin_val
{
    // Hack, generalise later
    if (name.starts_with("print") &&
        args.size() == 1 &&
        std::holds_alternative<type_list>(args[0]) &&
        inner_type(args[0]) == char_type()
    ) {
        const auto newline = name == "println";
        const auto length = std::get<type_list>(args[0]).count;
        return builtin_val{
            .ptr = [=](std::span<const std::byte> data) -> std::vector<std::byte> {
                for (const auto& datum : data) {
                    print("{}", static_cast<char>(datum));
                }
                if (newline) {
                    print("\n");
                }
                return std::vector{std::byte{0}};
            },
            .return_type = null_type()
        };
    }

    auto it = builtins.find({name, args});
    if (it == builtins.end()) {
        anzu::print("builtin error: could not find function '{}({})'\n", name, format_comma_separated(args));
        std::exit(1);
    }
    return it->second;
}

}