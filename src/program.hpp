#pragma once
#include "functions.hpp"
#include "object.hpp"

#include <variant>
#include <format>
#include <vector>
#include <string>
#include <string_view>

namespace anzu {

struct op_push_const
{
    anzu::object value;
};

struct op_push_var
{
    std::string name;
};

struct op_pop
{
};

// 0 == OP_DUP, 1 == OP_OVER, ...
struct op_copy_index
{
    int index;
};

struct op_store
{
    std::string name;
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

struct op_while
{
};

struct op_while_end
{
    std::intptr_t jump = -1;
};

struct op_for
{
};

struct op_for_end
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
    std::string        name;
    std::intptr_t      ptr;
    function_signature sig;
};

struct op_builtin_call
{
    std::string        name;
    builtin_function   ptr;
    function_signature sig;
};

struct op_add
{
};

struct op_sub
{
};

struct op_mul
{
};

struct op_div
{
};

struct op_mod
{
};

struct op_eq
{
};

struct op_ne
{
};

struct op_lt
{
};

struct op_le
{
};

struct op_gt
{
};

struct op_ge
{
};

struct op_or
{
};

struct op_and
{
};

struct op_function
{
    std::string        name;
    function_signature sig;
    std::intptr_t      jump;
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

struct op_debug
{

};

struct op : std::variant<
    op_push_const,
    op_push_var,
    op_pop,
    op_copy_index,
    op_store,
    op_if,
    op_if_end,
    op_else,
    op_while,
    op_while_end,
    op_for,
    op_for_end,
    op_break,
    op_continue,
    op_jump_if_false,
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_mod,
    op_eq,
    op_ne,
    op_lt,
    op_le,
    op_gt,
    op_ge,
    op_or,
    op_and,
    op_function,
    op_function_end,
    op_return,
    op_function_call,
    op_builtin_call,
    op_build_list,

    op_debug
>
{};

using program = std::vector<op>;

auto to_string(const op& op_code) -> std::string;
auto print_program(const anzu::program& program) -> void;

}