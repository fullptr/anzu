#pragma once
#include "stack_frame.hpp"

#include <fmt/format.h>
#include <variant>

namespace anzu {

constexpr auto FORMAT2 = std::string_view{"{:<30} {}"};
constexpr auto FORMAT3 = std::string_view{"{:<30} {:<20} {}"};

// Stack Manipulation

struct op_push_const
{
    anzu::object value;

    std::string to_string() const { return fmt::format("OP_PUSH_CONST({})", value.to_repr()); }
    void apply(anzu::context& ctx) const;
};

struct op_push_var
{
    std::string name;

    std::string to_string() const { return fmt::format("OP_PUSH_VAR({})", name); }
    void apply(anzu::context& ctx) const;
};

struct op_pop
{
    std::string to_string() const { return "OP_POP"; }
    void apply(anzu::context& ctx) const;
};

struct op_dup
{
    std::string to_string() const { return "OP_DUP"; }
    void apply(anzu::context& ctx) const;
};

struct op_swap
{
    std::string to_string() const { return "OP_SWAP"; }
    void apply(anzu::context& ctx) const;
};

struct op_rot
{
    std::string to_string() const { return "OP_ROT"; }
    void apply(anzu::context& ctx) const;
};

struct op_over
{
    std::string to_string() const { return "OP_OVER"; }
    void apply(anzu::context& ctx) const;
};

// Store Manipulation

struct op_store
{
    std::string name;

    std::string to_string() const { return fmt::format("OP_STORE({})", name); }
    void apply(anzu::context& ctx) const;
};

// Control Flow / Functions

struct op_if
{
    std::string to_string() const { return "OP_IF"; }
    void apply(anzu::context& ctx) const;
};

struct op_if_end
{
    std::string to_string() const { return "OP_END_IF"; }
    void apply(anzu::context& ctx) const;
};

struct op_elif
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = fmt::format("JUMP -> {}", jump);
        return fmt::format(FORMAT2, "OP_ELIF", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_else
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = fmt::format("JUMP -> {}", jump);
        return fmt::format(FORMAT2, "OP_ELSE", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_while
{
    std::string to_string() const { return "OP_WHILE"; }
    void apply(anzu::context& ctx) const;
};

struct op_while_end
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = fmt::format("JUMP -> {}", jump);
        return fmt::format(FORMAT2, "OP_END_WHILE", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_break
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = fmt::format("JUMP -> {}", jump);
        return fmt::format(FORMAT2, "OP_BREAK", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_continue
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = fmt::format("JUMP -> {}", jump);
        return fmt::format(FORMAT2, "OP_CONTINUE", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_do
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = fmt::format("JUMP -> {} IF FALSE", jump);
        return fmt::format(FORMAT2, "OP_DO", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_function
{
    std::string    name;
    std::intptr_t jump = -1;  // Jumps to end of function so it isnt invoked when running.

    std::string to_string() const
    {
        const auto func_str = fmt::format("OP_FUNCTION({})", name);
        const auto jump_str = fmt::format("JUMP -> {}", jump);
        return fmt::format(FORMAT2, func_str, jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_function_end
{
    int retc;

    std::string to_string() const
    {
        const auto retc_str = fmt::format("(RETC = {})", retc);
        return fmt::format(FORMAT3, "OP_FUNCTION_END", "JUMP -> CALL", retc_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_function_call
{
    std::string   name;
    int           argc;
    std::intptr_t jump;

    std::string to_string() const
    {
        const auto func_str = fmt::format("OP_FUNCTION_CALL({})", name);
        const auto jump_str = fmt::format("JUMP -> {}", jump);
        const auto argc_str = fmt::format("(ARGC = {})", argc);
        return fmt::format(FORMAT3, func_str, jump_str, argc_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_return
{
    int retc;

    std::string to_string() const
    {
        const auto retc_str = fmt::format("(RETC = {})", retc);
        return fmt::format(FORMAT3, "OP_RETURN", "JUMP -> CALL", retc_str);
    }
    void apply(anzu::context& ctx) const;
};

// Numerical Operators

struct op_add
{
    std::string to_string() const { return "OP_ADD"; }
    void apply(anzu::context& ctx) const;
};

struct op_sub
{
    std::string to_string() const { return "OP_SUB"; }
    void apply(anzu::context& ctx) const;
};

struct op_mul
{
    std::string to_string() const { return "OP_MUL"; }
    void apply(anzu::context& ctx) const;
};

struct op_div
{
    std::string to_string() const { return "OP_DIV"; }
    void apply(anzu::context& ctx) const;
};

struct op_mod
{
    std::string to_string() const { return "OP_MOD"; }
    void apply(anzu::context& ctx) const;
};

// Logical Operators

struct op_eq
{
    std::string to_string() const { return "OP_EQ"; }
    void apply(anzu::context& ctx) const;
};

struct op_ne
{
    std::string to_string() const { return "OP_NE"; }
    void apply(anzu::context& ctx) const;
};

struct op_lt
{
    std::string to_string() const { return "OP_LT"; }
    void apply(anzu::context& ctx) const;
};

struct op_le
{
    std::string to_string() const { return "OP_LE"; }
    void apply(anzu::context& ctx) const;
};

struct op_gt
{
    std::string to_string() const { return "OP_GT"; }
    void apply(anzu::context& ctx) const;
};

struct op_ge
{
    std::string to_string() const { return "OP_GE"; }
    void apply(anzu::context& ctx) const;
};

struct op_or
{
    std::string to_string() const { return "OP_OR"; }
    void apply(anzu::context& ctx) const;
};

struct op_and
{
    std::string to_string() const { return "OP_AND"; }
    void apply(anzu::context& ctx) const;
};

// Casts

struct op_to_int
{
    std::string to_string() const { return "OP_TO_INT"; }
    void apply(anzu::context& ctx) const;
};

struct op_to_bool
{
    std::string to_string() const { return "OP_TO_BOOL"; }
    void apply(anzu::context& ctx) const;
};

struct op_to_str
{
    std::string to_string() const { return "OP_STR"; }
    void apply(anzu::context& ctx) const;
};

// IO

struct op_input
{
    std::string to_string() const { return "OP_INPUT"; }
    void apply(anzu::context& ctx) const;
};

struct op_dump
{
    std::string to_string() const { return "OP_DUMP"; }
    void apply(anzu::context& ctx) const;
};

// Debug

struct op_print_frame
{
    std::string to_string() const { return "OP_PRINT_FRAME"; }
    void apply(anzu::context& ctx) const;
};

class op
{
    using op_type = std::variant<
        // Stack Manipulation
        op_push_const,
        op_push_var,
        op_pop,
        op_dup,
        op_swap,
        op_rot,
        op_over,

        // Store Manipulation
        op_store,

        // Control Flow
        op_if,
        op_if_end,
        op_elif,
        op_else,
        op_while,
        op_while_end,
        op_break,
        op_continue,
        op_do,

        // Functions
        op_function,
        op_function_call,
        op_function_end,
        op_return,

        // Numerical Operators
        op_add,
        op_sub,
        op_mul,
        op_div,
        op_mod,

        // Logical Operators
        op_eq,
        op_ne,
        op_lt,
        op_le,
        op_gt,
        op_ge,
        op_or,
        op_and,

        // Casts
        op_to_int,
        op_to_bool,
        op_to_str,

        // IO
        op_input,
        op_dump,

        // Debug
        op_print_frame
    >;

    op_type d_type;

public:
    template <typename Op>
    op(const Op& op_type) : d_type(op_type) {}

    template <typename Op>
    Op* get_if() { return std::get_if<Op>(&d_type); }

    inline std::string to_string() const {
        return std::visit([](auto&& o) { return o.to_string(); }, d_type);
    }

    inline void apply(anzu::context& ctx) const {
        return std::visit([&](auto&& o) { o.apply(ctx); }, d_type);
    }
};

}

template <> struct fmt::formatter<anzu::op> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.end(); }
    auto format(const anzu::op& op, auto& ctx) {
        return format_to(ctx.out(), "{}", op.to_string());
    }
};