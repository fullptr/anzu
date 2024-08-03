#include "functions.hpp"
#include "object.hpp"
#include "runtime.hpp"
#include "utility/common.hpp"
#include "utility/memory.hpp"

#include <unordered_map>
#include <string>
#include <functional>
#include <utility>

namespace anzu {
namespace {

auto pop_char_span(bytecode_context& ctx) -> std::string
{
    const auto size = ctx.stack.pop<std::uint64_t>();
    const auto ptr = ctx.stack.pop<std::byte*>();
    
    auto ret = std::string(size, ' ');
    std::memcpy(ret.data(), ptr, size);
    return ret;
}

auto pop_arena(bytecode_context& ctx) -> memory_arena*
{
    // we pushed a pointer to an arena, which is a pointer in C++
    return *ctx.stack.pop<memory_arena**>();
}

auto builtin_sqrt(bytecode_context& ctx) -> void
{
    auto val = ctx.stack.pop<double>();
    ctx.stack.push(std::sqrt(val));
}

static_assert(sizeof(std::FILE*) == sizeof(std::uint64_t));

auto builtin_fopen(bytecode_context& ctx) -> void
{
    const auto mode = pop_char_span(ctx);
    const auto file = pop_char_span(ctx);
    const auto handle = std::fopen(file.c_str(), mode.c_str());
    ctx.stack.push(handle);
}

auto builtin_fclose(bytecode_context& ctx) -> void
{
    const auto ptr = ctx.stack.pop<std::FILE*>();
    std::fclose(ptr);
    ctx.stack.push(std::byte{0}); // returns null
}

auto builtin_fputs(bytecode_context& ctx) -> void
{
    const auto data = pop_char_span(ctx);
    const auto ptr = ctx.stack.pop<std::FILE*>();
    std::fputs(data.c_str(), ptr);
    ctx.stack.push(std::byte{0}); // returns null
}

auto builtin_fread(bytecode_context& ctx) -> void
{
    auto arena = pop_arena(ctx);
    auto file = ctx.stack.pop<std::FILE*>();
    std::fseek(file, 0, SEEK_END);
    const auto ssize = std::ftell(file);
    if (ssize == -1) {
        std::print("Error with ftell\n");
        std::exit(1);
    }
    const auto size = static_cast<std::size_t>(ssize);
    std::rewind(file);
    std::byte* ptr = &arena->data[arena->next];
    std::fread(ptr, sizeof(std::byte), ssize, file);
    arena->next += size;
    ctx.stack.push(ptr); // push the span
    ctx.stack.push(size);
}

auto builtin_span_from_ptrs(bytecode_context& ctx) -> void
{
    auto end = ctx.stack.pop<const char*>();
    auto start = ctx.stack.pop<const char*>();
    const auto size = static_cast<std::size_t>(end - start);
    ctx.stack.push(start);
    ctx.stack.push(size);
}

}

auto construct_builtin_array() -> std::vector<builtin>
{
    const auto char_span = char_type().add_const().add_span();
    const auto char_ptr = char_type().add_const().add_ptr();

    auto b = std::vector<builtin>{};

    b.push_back(builtin{"sqrt", builtin_sqrt, {f64_type()}, f64_type()});

    b.push_back(builtin{"fopen", builtin_fopen, {char_span, char_span}, u64_type()});
    b.push_back(builtin{"fclose", builtin_fclose, {u64_type()}, null_type()});
    b.push_back(builtin{"fputs", builtin_fputs, {u64_type(), char_span}, null_type()});
    b.push_back(builtin{"fread", builtin_fread, {u64_type(), arena_type().add_ptr()}, char_span});

    b.push_back(builtin{"span_from_ptrs", builtin_span_from_ptrs, {char_ptr, char_ptr}, char_span});

    return b;
}

static const auto builtins = construct_builtin_array();

auto get_builtins() -> std::span<const builtin>
{
    return builtins;
}

auto get_builtin(const std::string& name) -> std::pair<const builtin*, std::size_t>
{
    auto index = std::size_t{0};
    for (const auto& b : get_builtins()) {
        if (name == b.name) {
            return {&b, index};
        }
        ++index;
    }
    return {nullptr, 0};
}

}