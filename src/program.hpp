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
    object_def value;
};

struct op_load_global
{
    std::string name;
    std::size_t position;
};

struct op_load_local
{
    std::string name;
    std::size_t offset;
};

struct op_pop
{
};

struct op_save_global
{
    std::string name;
    std::size_t position;
};

struct op_save_local
{
    std::string name;
    std::size_t offset;
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

// Will replace all of the other binary operator op codes. Switching to this way since the
// types are known at compile time, and will also allow for custom operators for class types.
struct op_builtin_bin_op
{
    // Debug info, can we do this better?
    std::string      lhs;
    std::string      rhs;
    std::string      op;

    builtin_function ptr;
    signature        sig; // TODO: Remove from op
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
    op_build_list
>
{};

using program = std::vector<op>;

auto to_string(const op& op_code) -> std::string;
auto print_program(const anzu::program& program) -> void;

}