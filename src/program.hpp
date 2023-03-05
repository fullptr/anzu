#pragma once
#include "functions.hpp"
#include "object.hpp"

#include <variant>
#include <format>
#include <vector>
#include <utility>
#include <string>
#include <string_view>

namespace anzu {

struct op_push_literal_i32
{
    std::int32_t value;
};

struct op_push_literal_i64
{
    std::int64_t value;
};

struct op_push_literal_u64
{
    std::uint64_t value;
};

struct op_push_literal_f64
{
    double value;
};

struct op_push_literal_char
{
    char value;
};

struct op_push_literal_bool
{
    bool value;
};

struct op_push_literal_null
{
};

struct op_push_global_addr
{
    std::size_t position;
};

struct op_push_local_addr
{
    std::size_t offset;
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

struct op_alloc_span
{
    std::size_t type_size;
};

struct op_dealloc_span
{
    std::size_t type_size;
};

struct op_alloc_ptr
{
    std::size_t type_size;
};

struct op_dealloc_ptr
{
    std::size_t type_size;
};

struct op_jump
{
    std::size_t jump;
};

struct op_jump_if_false
{
    std::size_t jump;
};

struct op_function_call
{
    std::size_t ptr;
    std::size_t args_size;
};

// Same as op_function_call, but fetches the pointer from the stack
struct op_call
{
    std::size_t args_size;
};

struct op_builtin_call
{
    std::string      name;
    builtin_function ptr;
    std::size_t      args_size;
};

struct op_return
{
    std::size_t size;
};

struct op_debug
{
    std::string message;
};

struct op_assert
{
    std::string message;
};

struct op_char_eq {};
struct op_char_ne {};

struct op_i32_add {};
struct op_i32_sub {};
struct op_i32_mul {};
struct op_i32_div {};
struct op_i32_mod {};
struct op_i32_eq {};
struct op_i32_ne {};
struct op_i32_lt {};
struct op_i32_le {};
struct op_i32_gt {};
struct op_i32_ge {};

struct op_i64_add {};
struct op_i64_sub {};
struct op_i64_mul {};
struct op_i64_div {};
struct op_i64_mod {};
struct op_i64_eq {};
struct op_i64_ne {};
struct op_i64_lt {};
struct op_i64_le {};
struct op_i64_gt {};
struct op_i64_ge {};

struct op_u64_add {};
struct op_u64_sub {};
struct op_u64_mul {};
struct op_u64_div {};
struct op_u64_mod {};
struct op_u64_eq {};
struct op_u64_ne {};
struct op_u64_lt {};
struct op_u64_le {};
struct op_u64_gt {};
struct op_u64_ge {};

struct op_f64_add {};
struct op_f64_sub {};
struct op_f64_mul {};
struct op_f64_div {};
struct op_f64_eq {};
struct op_f64_ne {};
struct op_f64_lt {};
struct op_f64_le {};
struct op_f64_gt {};
struct op_f64_ge {};

struct op_bool_or {};
struct op_bool_and {};
struct op_bool_eq {};
struct op_bool_ne {};
struct op_bool_not {};

struct op_i32_neg {};
struct op_i64_neg {};
struct op_f64_neg {};

struct op : std::variant<
    op_push_literal_i32,
    op_push_literal_i64,
    op_push_literal_u64,
    op_push_literal_f64,
    op_push_literal_char,
    op_push_literal_bool,
    op_push_literal_null,

    op_push_global_addr,
    op_push_local_addr,
    op_load,
    op_save,
    op_pop,
    op_alloc_span,
    op_dealloc_span,
    op_alloc_ptr,
    op_dealloc_ptr,
    op_jump,
    op_jump_if_false,
    op_return,
    op_function_call,
    op_call,
    op_builtin_call,
    op_assert,
    op_debug,

    op_char_eq,
    op_char_ne,

    op_i32_add,
    op_i32_sub,
    op_i32_mul,
    op_i32_div,
    op_i32_mod,
    op_i32_eq,
    op_i32_ne,
    op_i32_lt,
    op_i32_le,
    op_i32_gt,
    op_i32_ge,

    op_i64_add,
    op_i64_sub,
    op_i64_mul,
    op_i64_div,
    op_i64_mod,
    op_i64_eq,
    op_i64_ne,
    op_i64_lt,
    op_i64_le,
    op_i64_gt,
    op_i64_ge,

    op_u64_add,
    op_u64_sub,
    op_u64_mul,
    op_u64_div,
    op_u64_mod,
    op_u64_eq,
    op_u64_ne,
    op_u64_lt,
    op_u64_le,
    op_u64_gt,
    op_u64_ge,

    op_f64_add,
    op_f64_sub,
    op_f64_mul,
    op_f64_div,
    op_f64_eq,
    op_f64_ne,
    op_f64_lt,
    op_f64_le,
    op_f64_gt,
    op_f64_ge,

    op_bool_and,
    op_bool_or,
    op_bool_eq,
    op_bool_ne,
    op_bool_not,

    op_i32_neg,
    op_i64_neg,
    op_f64_neg
>
{};

struct program
{
    std::vector<op>        code;
    std::vector<std::byte> rom;
};

auto to_string(const op& op_code) -> std::string;
auto print_program(const anzu::program& program) -> void;

}

template <> struct std::formatter<anzu::op> : std::formatter<std::string> {
    auto format(const anzu::op& op, auto& ctx) {
        return std::formatter<std::string>::format(to_string(op), ctx);
    }
};