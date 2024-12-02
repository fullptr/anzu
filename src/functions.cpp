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

static_assert(sizeof(std::FILE*) == sizeof(std::uint64_t));

auto builtin_native_read_file(bytecode_context& ctx) -> void
{
    auto arena = pop_arena(ctx);
    const auto file = pop_char_span(ctx);
    const auto handle = std::fopen(file.c_str(), "rb");

    std::fseek(handle, 0, SEEK_END);
    const auto ssize = std::ftell(handle);
    if (ssize == -1) {
        std::print("Error with ftell\n");
        std::exit(1);
    }
    const auto size = static_cast<std::size_t>(ssize);
    std::rewind(handle);
    std::byte* ptr = &arena->data[arena->next];
    const auto bytes_read = std::fread(ptr, sizeof(std::byte), ssize, handle);
    if (bytes_read != ssize) {
        std::print("Error with fread\n");
	    std::exit(1);
    }	
    arena->next += size;

    std::fclose(handle);
    ctx.stack.push(ptr);  // push the
    ctx.stack.push(size); // span
}

}

auto construct_builtin_array() -> std::vector<builtin>
{
    const auto char_span = type_name{type_char{}}.add_const().add_span();
    const auto char_ptr = type_name{type_char{}}.add_const().add_ptr();

    auto b = std::vector<builtin>{};

    b.push_back(builtin{"native_read_file", b.size(), builtin_native_read_file, {char_span, type_name{type_arena{}}.add_ptr()}, char_span});
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
