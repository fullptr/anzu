#include "functions.hpp"
#include "object.hpp"
#include "runtime.hpp"
#include "utility/common.hpp"
#include "utility/memory.hpp"

#include <unordered_map>
#include <string>
#include <functional>
#include <utility>
#include <cmath>

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
    const auto bytes_read = std::fread(ptr, sizeof(std::byte), ssize, file);
    if (bytes_read != ssize) {
        std::print("Error with fread\n");
	std::exit(1);
    }	
    arena->next += size;
    ctx.stack.push(ptr); // push the span
    ctx.stack.push(size);
}

}

auto construct_builtin_array() -> std::vector<builtin>
{
    const auto char_span = type_name{type_char{}}.add_const().add_span();
    const auto char_ptr = type_name{type_char{}}.add_const().add_ptr();

    auto b = std::vector<builtin>{};

    b.push_back(builtin{"sqrt", b.size(), builtin_sqrt, {type_f64{}}, type_f64{}});

    b.push_back(builtin{"fopen", b.size(), builtin_fopen, {char_span, char_span}, {type_u64{}}});
    b.push_back(builtin{"fclose", b.size(), builtin_fclose, {type_u64{}}, {type_null{}}});
    b.push_back(builtin{"fputs", b.size(), builtin_fputs, {type_u64{}, char_span}, type_null{}});
    b.push_back(builtin{"fread", b.size(), builtin_fread, {type_u64{}, type_name{type_arena{}}.add_ptr()}, char_span});

    return b;
}

static const auto builtins = construct_builtin_array();

auto get_builtin(const std::string& name) -> const builtin*
{
    auto index = std::size_t{0};
    for (const auto& b : builtins) {
        if (name == b.name) {
            return &b;
        }
        ++index;
    }
    return nullptr;
}

auto get_builtin(std::size_t id) -> const builtin*
{
    return &builtins[id];
}

}
