#include "functions.hpp"
#include "object.hpp"
#include "bytecode.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/memory.hpp"

#include <unordered_map>
#include <string>
#include <functional>
#include <utility>

namespace anzu {
namespace {

auto resolve_ptr(bytecode_context& ctx, std::uint64_t ptr) -> std::byte*
{
    if (is_heap_ptr(ptr)) {
        const auto index = unset_heap_bit(ptr);
        return &ctx.heap[index];
    }
    if (is_rom_ptr(ptr)) {
        const auto index = unset_rom_bit(ptr);
        return &ctx.rom[index];
    }
    const auto index = ptr;
    return &ctx.stack[index];
}

auto pop_char_span(bytecode_context& ctx) -> std::string
{
    const auto size = pop_value<std::uint64_t>(ctx.stack);
    const auto ptr = pop_value<std::uint64_t>(ctx.stack);
    const auto real_ptr = resolve_ptr(ctx, ptr);
    
    auto ret = std::string(size, ' ');
    std::memcpy(ret.data(), real_ptr, size);
    return ret;
}

auto builtin_sqrt(bytecode_context& ctx) -> void
{
    auto val = pop_value<double>(ctx.stack);
    push_value(ctx.stack, std::sqrt(val));
}

auto builtin_print_char(bytecode_context& ctx) -> void
{
    print("{}", static_cast<char>(ctx.stack.back()));
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_println_char(bytecode_context& ctx) -> void
{
    print("{}\n", static_cast<char>(ctx.stack.back()));
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_print_bool(bytecode_context& ctx) -> void
{
    print("{}", ctx.stack.back() == std::byte{1});
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_println_bool(bytecode_context& ctx) -> void
{
    print("{}\n", ctx.stack.back() == std::byte{1});
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_print_null(bytecode_context& ctx) -> void
{
    print("null");
    ctx.stack.back() = std::byte{0}; // returns null
}

auto builtin_println_null(bytecode_context& ctx) -> void
{
    print("null\n");
    ctx.stack.back() = std::byte{0}; // returns null
}

template <typename T>
auto builtin_print(bytecode_context& ctx) -> void
{
    print("{}", pop_value<T>(ctx.stack));
    ctx.stack.push_back(std::byte{0}); // returns null
}

template <typename T>
auto builtin_println(bytecode_context& ctx) -> void
{
    print("{}\n", pop_value<T>(ctx.stack));
    ctx.stack.push_back(std::byte{0}); // returns null
}

auto builtin_print_char_span(bytecode_context& ctx) -> void
{
    const auto size = pop_value<std::uint64_t>(ctx.stack);
    const auto ptr = pop_value<std::uint64_t>(ctx.stack);
    const auto real_ptr = resolve_ptr(ctx, ptr);
    for (std::size_t i = 0; i != size; ++i) {
        print("{}", static_cast<char>(real_ptr[i]));
    }
    ctx.stack.push_back(std::byte{0}); // returns null
}

auto builtin_println_char_span(bytecode_context& ctx) -> void
{
    const auto size = pop_value<std::uint64_t>(ctx.stack);
    const auto ptr = pop_value<std::uint64_t>(ctx.stack);
    const auto real_ptr = resolve_ptr(ctx, ptr);
    for (std::size_t i = 0; i != size; ++i) {
        print("{}", static_cast<char>(real_ptr[i]));
    }
    print("\n");
    ctx.stack.push_back(std::byte{0}); // returns null
}

static_assert(sizeof(std::FILE*) == sizeof(std::uint64_t));

auto builtin_fopen(bytecode_context& ctx) -> void
{
    const auto mode = pop_char_span(ctx);
    const auto file = pop_char_span(ctx);
    const auto handle = std::fopen(file.c_str(), mode.c_str());
    push_value(ctx.stack, handle);
}

auto builtin_fclose(bytecode_context& ctx) -> void
{
    const auto ptr = pop_value<std::FILE*>(ctx.stack);
    std::fclose(ptr);
    ctx.stack.push_back(std::byte{0}); // returns null
}

auto builtin_fputs(bytecode_context& ctx) -> void
{
    const auto data = pop_char_span(ctx);
    const auto ptr = pop_value<std::FILE*>(ctx.stack);
    std::fputs(data.c_str(), ptr);
    ctx.stack.push_back(std::byte{0}); // returns null
}

}

auto construct_builtin_array() -> std::vector<builtin>
{
    const auto char_span = concrete_span_type(char_type().add_const()).add_const();

    auto b = std::vector<builtin>{};

    b.push_back(builtin{"sqrt", builtin_sqrt, {f64_type()}, f64_type()});

    b.push_back(builtin{"print", builtin_print<std::int32_t>,  {i32_type()},  null_type()});
    b.push_back(builtin{"print", builtin_print<std::int64_t>,  {i64_type()},  null_type()});
    b.push_back(builtin{"print", builtin_print<std::uint64_t>, {u64_type()},  null_type()});
    b.push_back(builtin{"print", builtin_print<double>,        {f64_type()},  null_type()});
    b.push_back(builtin{"print", builtin_print_char,           {char_type()}, null_type()});
    b.push_back(builtin{"print", builtin_print_bool,           {bool_type()}, null_type()});
    b.push_back(builtin{"print", builtin_print_null,           {null_type()}, null_type()});
    b.push_back(builtin{"print", builtin_print_char_span,      {char_span},   null_type()});

    b.push_back(builtin{"println", builtin_println<std::int32_t>,  {i32_type()},  null_type()});
    b.push_back(builtin{"println", builtin_println<std::int64_t>,  {i64_type()},  null_type()});
    b.push_back(builtin{"println", builtin_println<std::uint64_t>, {u64_type()},  null_type()});
    b.push_back(builtin{"println", builtin_println<double>,        {f64_type()},  null_type()});
    b.push_back(builtin{"println", builtin_println_char,           {char_type()}, null_type()});
    b.push_back(builtin{"println", builtin_println_bool,           {bool_type()}, null_type()});
    b.push_back(builtin{"println", builtin_println_null,           {null_type()}, null_type()});
    b.push_back(builtin{"println", builtin_println_char_span,      {char_span},   null_type()});

    b.push_back(builtin{"fopen", builtin_fopen, {char_span, char_span}, u64_type()});
    b.push_back(builtin{"fclose", builtin_fclose, {u64_type()}, null_type()});
    b.push_back(builtin{"fputs", builtin_fputs, {u64_type(), char_span}, null_type()});

    return b;
}

static const auto builtins = construct_builtin_array();

auto get_builtin_id(const std::string& name, const std::vector<type_name>& args)
    -> std::optional<std::size_t>
{
    auto index = std::size_t{0};
    for (const auto& b : builtins) {
        if (name == b.name && are_types_convertible_to(args, b.args)) {
            return index;
        }
        ++index;
    }
    return std::nullopt;
}

auto get_builtin(std::size_t id) -> const builtin&
{
    return builtins[id];
}

}