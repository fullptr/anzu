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

auto pop_char_span(runtime_context& ctx) -> std::string
{
    const auto size = pop_value<std::uint64_t>(ctx.stack);
    const auto ptr = pop_value<std::uint64_t>(ctx.stack);
    
    auto ret = std::string(size, ' ');
    ret.resize(size);
    
    if (is_heap_ptr(ptr)) {
        const auto index = unset_heap_bit(ptr);
        std::memcpy(ret.data(), &ctx.heap[index], size);
    }
    else if (is_rom_ptr(ptr)) {
        const auto index = unset_rom_bit(ptr);
        std::memcpy(ret.data(), &ctx.rom[index], size);
    }
    else {
        const auto index = ptr;
        std::memcpy(ret.data(), &ctx.stack[index], size);
    }
    return ret;
}

auto builtin_sqrt(runtime_context& ctx) -> void
{
    auto val = pop_value<double>(ctx.stack);
    push_value(ctx.stack, std::sqrt(val));
}

auto builtin_print_char(runtime_context& ctx) -> void
{
    print("{}", static_cast<char>(ctx.stack.back()));
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_println_char(runtime_context& ctx) -> void
{
    print("{}\n", static_cast<char>(ctx.stack.back()));
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_print_bool(runtime_context& ctx) -> void
{
    print("{}", ctx.stack.back() == std::byte{1});
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_println_bool(runtime_context& ctx) -> void
{
    print("{}\n", ctx.stack.back() == std::byte{1});
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_print_null(runtime_context& ctx) -> void
{
    print("null");
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_println_null(runtime_context& ctx) -> void
{
    print("null\n");
    ctx.stack.back() = std::byte{0}; // returns null
}

template <typename T>
auto builtin_print(runtime_context& ctx) -> void
{
    print("{}", pop_value<T>(ctx.stack));
    ctx.stack.push_back(std::byte{0}); // returns null
}

template <typename T>
auto builtin_println(runtime_context& ctx) -> void
{
    print("{}\n", pop_value<T>(ctx.stack));
    ctx.stack.push_back(std::byte{0}); // returns null
}

static_assert(sizeof(std::FILE*) == sizeof(std::uint64_t));

auto builtin_fopen(runtime_context& ctx) -> void
{
    const auto mode = pop_char_span(ctx);
    const auto file = pop_char_span(ctx);
    const auto ptr = std::fopen(file.c_str(), mode.c_str());
    push_value<std::FILE*>(ctx.stack, ptr);
}

auto builtin_fclose(runtime_context& ctx) -> void
{
    const auto ptr = pop_value<std::FILE*>(ctx.stack);
    std::fclose(ptr);
    ctx.stack.push_back(std::byte{0}); // returns null
}

auto builtin_fputs(runtime_context& ctx) -> void
{
    const auto data = pop_char_span(ctx);
    const auto ptr = pop_value<std::FILE*>(ctx.stack);
    std::fputs(data.c_str(), ptr);
    ctx.stack.push_back(std::byte{0}); // returns null
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

    const auto char_span = concrete_span_type(char_type());

    builtins.emplace(
        builtin_key{ .name = "fopen", .args = { char_span, char_span }},
        builtin_val{ .ptr = builtin_fopen, .return_type = u64_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "fclose", .args = { u64_type() }},
        builtin_val{ .ptr = builtin_fclose, .return_type = null_type() }
    );

    builtins.emplace(
        builtin_key{ .name = "fputs", .args = { u64_type(), char_span }},
        builtin_val{ .ptr = builtin_fputs, .return_type = null_type() }
    );

    return builtins;
}

static const auto builtins = construct_builtin_map();

auto is_builtin(const std::string& name, const std::vector<type_name>& args) -> bool
{
    // Hack, generalise later
    if ((name == "print" || name == "println") &&
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
        std::holds_alternative<type_ptr>(args[0])
    ) {
        const auto newline = name == "println";
        return builtin_val{
            .ptr = [=](runtime_context& ctx) -> void {
                const auto ptr = pop_value<std::uint64_t>(ctx.stack);
                print("{}", ptr);
                if (newline) {
                    print("\n");
                }
                ctx.stack.push_back(std::byte{0}); // Return null
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