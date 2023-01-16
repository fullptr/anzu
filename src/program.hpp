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

struct op_i32_add {};
struct op_i32_sub {};
struct op_i32_mul {};
struct op_i32_div {};
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

struct op_allocate
{
    std::size_t type_size;
};

struct op_deallocate
{
};

struct op_jump
{
    std::int64_t jump;
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

struct op_function
{
    std::string name;
    std::size_t jump;
};

struct op_return
{
    std::size_t size;
};

struct op_debug
{
    std::string message;
};

struct op : std::variant<
    op_load_bytes,
    op_push_global_addr,
    op_push_local_addr,

    op_i32_add,
    op_i32_sub,
    op_i32_mul,
    op_i32_div,
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

    op_load,
    op_save,
    op_pop,
    op_allocate,
    op_deallocate,
    op_jump,
    op_jump_if_false,
    op_function,
    op_return,
    op_function_call,
    op_builtin_call,
    op_debug
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