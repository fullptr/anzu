#pragma once
#include "functions.hpp"
#include "object.hpp"

#include <variant>
#include <format>
#include <vector>
#include <string>
#include <string_view>

namespace anzu {

struct op_load_literal
{
    object value;
};

struct op_load_global
{
    std::string name;
    std::size_t position;
    std::size_t size;
};

struct op_load_local
{
    std::string name;
    std::size_t offset;
    std::size_t size;
};

struct op_pop
{
};

struct op_save_global
{
    std::string name;
    std::size_t position;
    std::size_t size;
};

struct op_save_local
{
    std::string name;
    std::size_t offset;
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
    std::intptr_t jump = -1;
};

struct op_loop_begin
{
};

struct op_loop_end
{
    std::intptr_t jump = -1;
};

struct op_break
{
    std::intptr_t jump = -1;
};

struct op_continue
{
    std::intptr_t jump = -1;
};

struct op_jump_if_false
{
    std::intptr_t jump = -1;
};

struct op_function_call
{
    std::string   name;
    std::intptr_t ptr;
    signature     sig;
};

struct op_builtin_call
{
    std::string      name;
    builtin_function ptr;
    signature        sig; // TODO: Remove from op
};

struct op_builtin_mem_op
{
    std::string    name;
    builtin_mem_op ptr;
};

struct op_function
{
    std::string   name;
    signature     sig;
    std::intptr_t jump;
};

struct op_function_end
{
};

struct op_return
{
};

struct op_build_list
{
    std::size_t size;
};

struct op : std::variant<
    op_load_literal,
    op_load_global,
    op_load_local,
    op_pop,
    op_save_global,
    op_save_local,
    op_if,
    op_if_end,
    op_else,
    op_loop_begin,
    op_loop_end,
    op_break,
    op_continue,
    op_jump_if_false,
    op_builtin_mem_op,
    op_function,
    op_function_end,
    op_return,
    op_function_call,
    op_builtin_call,
    op_build_list
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