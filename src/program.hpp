#pragma once
#include "functions.hpp"
#include "operators.hpp"
#include "object.hpp"

#include <variant>
#include <format>
#include <vector>
#include <utility>
#include <string>
#include <string_view>

namespace anzu {

struct op_load_bytes
{
    std::vector<std::byte> bytes;
};

struct op_push_global_addr
{
    std::size_t position;
};

struct op_push_local_addr
{
    std::size_t offset;
};

// This is just integer addition now
struct op_modify_ptr
{
};

struct op_load
{
    std::size_t size;
};

struct op_save
{
    std::size_t size;
};

struct op_pop
{
    std::size_t size;
};

struct op_if
{
};

struct op_if_end
{
};

struct op_else
{
    std::size_t jump;
};

struct op_jump_relative
{
    std::int64_t jump;
};

struct op_break
{
    std::size_t jump;
};

struct op_continue
{
    std::size_t jump;
};

struct op_jump_if_false
{
    std::size_t jump;
};

struct op_function_call
{
    std::string name;
    std::size_t ptr;
    std::size_t args_size;
};

struct op_builtin_call
{
    std::string      name;
    builtin_function ptr;
    std::size_t      args_size;
};

struct op_builtin_mem_op
{
    std::string    name;
    builtin_mem_op ptr;
};

struct op_function
{
    std::string name;
    std::size_t jump;
};

struct op_return
{
};

struct op : std::variant<
    op_load_bytes,
    op_push_global_addr,
    op_push_local_addr,
    op_modify_ptr,
    op_load,
    op_save,
    op_pop,
    op_if,
    op_if_end,
    op_else,
    op_jump_relative,
    op_break,
    op_continue,
    op_jump_if_false,
    op_builtin_mem_op,
    op_function,
    op_return,
    op_function_call,
    op_builtin_call
>
{};

using program = std::vector<op>;

auto to_string(const op& op_code) -> std::string;
auto print_program(const anzu::program& program) -> void;

}

template <> struct std::formatter<anzu::op> : std::formatter<std::string> {
    auto format(const anzu::op& op, auto& ctx) {
        return std::formatter<std::string>::format(to_string(op), ctx);
    }
};