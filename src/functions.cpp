#include "functions.hpp"
#include "object.hpp"
#include "runtime.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/memory.hpp"

#include <unordered_map>
#include <string>
#include <functional>
#include <utility>

namespace anzu {
namespace {

auto builtin_sqrt(std::vector<std::byte>& mem) -> void
{
    auto val = pop_value<double>(mem);
    push_value(mem, std::sqrt(val));
}

auto builtin_print_char(std::vector<std::byte>& mem) -> void
{
    print("{}", static_cast<char>(mem.back()));
    mem.back() = std::byte{0}; // returns null
}

auto builtin_println_char(std::vector<std::byte>& mem) -> void
{
    print("{}\n", static_cast<char>(mem.back()));
    mem.back() = std::byte{0}; // returns null
}

auto builtin_print_bool(std::vector<std::byte>& mem) -> void
{
    print("{}", mem.back() == std::byte{1});
    mem.back() = std::byte{0}; // returns null
}

auto builtin_println_bool(std::vector<std::byte>& mem) -> void
{
    print("{}\n", mem.back() == std::byte{1});
    mem.back() = std::byte{0}; // returns null
}

auto builtin_print_null(std::vector<std::byte>& mem) -> void
{
    print("null");
    mem.back() = std::byte{0}; // returns null
}

auto builtin_println_null(std::vector<std::byte>& mem) -> void
{
    print("null\n");
    mem.back() = std::byte{0}; // returns null
}

template <typename T>
auto builtin_print(std::vector<std::byte>& mem) -> void
{
    print("{}", pop_value<T>(mem));
    mem.push_back(std::byte{0}); // returns null
}

template <typename T>
auto builtin_println(std::vector<std::byte>& mem) -> void
{
    print("{}\n", pop_value<T>(mem));
    mem.push_back(std::byte{0}); // returns null
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
        builtin_val{ .ptr = builtin_print<std::uint64_t>, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { u64_type() } },
        builtin_val{ .ptr = builtin_println<std::uint64_t>, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { char_type() } },
        builtin_val{ .ptr = builtin_print<char>, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { char_type() } },
        builtin_val{ .ptr = builtin_println<char>, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { f64_type() } },
        builtin_val{ .ptr = builtin_print<double>, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { f64_type() } },
        builtin_val{ .ptr = builtin_println<double>, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { bool_type() } },
        builtin_val{ .ptr = builtin_print<bool>, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { bool_type() } },
        builtin_val{ .ptr = builtin_println<bool>, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { null_type() } },
        builtin_val{ .ptr = builtin_print<std::byte>, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { null_type() } },
        builtin_val{ .ptr = builtin_println<std::byte>, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { i32_type() } },
        builtin_val{ .ptr = builtin_print<std::int32_t>, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { i32_type() } },
        builtin_val{ .ptr = builtin_println<std::int32_t>, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "print", .args = { i64_type() } },
        builtin_val{ .ptr = builtin_print<std::int64_t>, .return_type = null_type() }
    );
    builtins.emplace(
        builtin_key{ .name = "println", .args = { i64_type() } },
        builtin_val{ .ptr = builtin_println<std::int64_t>, .return_type = null_type() }
    );

    return builtins;
}

static const auto builtins = construct_builtin_map();

auto is_builtin(const std::string& name, const std::vector<type_name>& args) -> bool
{
    // Hack, generalise later
    if ((name == "print" || name == "println") &&
        args.size() == 1 &&
        std::holds_alternative<type_list>(args[0]) &&
        inner_type(args[0]) == char_type()
    ) {
        return true;
    }
    else if ((name == "print" || name == "println") &&
        args.size() == 1 &&
        std::holds_alternative<type_ptr>(args[0])
    ) {
        return true;
    }
    return builtins.contains({name, args});
}

auto fetch_builtin(const std::string& name, const std::vector<type_name>& args) -> builtin_val
{
    // Hack, generalise later
    if ((name == "print" || name == "println") &&
        args.size() == 1 &&
        std::holds_alternative<type_list>(args[0]) &&
        inner_type(args[0]) == char_type()
    ) {
        const auto newline = name == "println";
        const auto length = std::get<type_list>(args[0]).count;
        return builtin_val{
            .ptr = [=](std::vector<std::byte>& mem) -> void {
                auto it = mem.end();
                std::advance(it, -1 * length);
                for (; it != mem.end(); ++it) {
                    print("{}", static_cast<char>(*it));
                }
                if (newline) {
                    print("\n");
                }
                pop_n(mem, length);
                mem.push_back(std::byte{0}); // Return null
            },
            .return_type = null_type()
        };
    }
    else if ((name == "print" || name == "println") &&
        args.size() == 1 &&
        std::holds_alternative<type_ptr>(args[0])
    ) {
        const auto newline = name == "println";
        return builtin_val{
            .ptr = [=](std::vector<std::byte>& mem) -> void {
                const auto ptr = pop_value<std::uint64_t>(mem);
                print("{}", ptr);
                if (newline) {
                    print("\n");
                }
                mem.push_back(std::byte{0}); // Return null
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