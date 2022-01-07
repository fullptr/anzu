#pragma once
#include "stack_frame.hpp"

#include <fmt/format.h>
#include <variant>

namespace anzu {

constexpr auto PRINT_JUMP          = std::string_view{"{:<25} JUMP -> {}"};
constexpr auto PRINT_JUMP_IF_FALSE = std::string_view{"{:<25} JUMP -> {} (IF FALSE)"};

struct op_store
{
    std::string name;

    std::string to_string() const { return fmt::format("OP_STORE({})", name); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_dump
{
    std::string to_string() const { return fmt::format("OP_DUMP"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_pop
{
    std::string to_string() const { return fmt::format("OP_POP"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_push_const
{
    anzu::object value;

    std::string to_string() const { return fmt::format("OP_PUSH_CONST({})", value); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_push_var
{
    std::string name;

    std::string to_string() const { return fmt::format("OP_PUSH_VAR({})", name); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_add
{
    std::string to_string() const { return fmt::format("OP_ADD"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_sub
{
    std::string to_string() const { return fmt::format("OP_SUB"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_mul
{
    std::string to_string() const { return fmt::format("OP_MUL"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_div
{
    std::string to_string() const { return fmt::format("OP_DIV"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_mod
{
    std::string to_string() const { return fmt::format("OP_MOD"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_dup
{
    std::string to_string() const { return fmt::format("OP_DUP"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_print_frame
{
    std::string to_string() const { return fmt::format("OP_PRINT_FRAME"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_eq
{
    std::string to_string() const { return fmt::format("OP_EQ"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_ne
{
    std::string to_string() const { return fmt::format("OP_NE"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_lt
{
    std::string to_string() const { return fmt::format("OP_LT"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_le
{
    std::string to_string() const { return fmt::format("OP_LE"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_gt
{
    std::string to_string() const { return fmt::format("OP_GT"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_ge
{
    std::string to_string() const { return fmt::format("OP_GE"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_or
{
    std::string to_string() const { return fmt::format("OP_OR"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_and
{
    std::string to_string() const { return fmt::format("OP_AND"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_input
{
    std::string to_string() const { return fmt::format("OP_INPUT"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_if
{
    std::string to_string() const { return fmt::format("OP_IF"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_end_if
{
    std::string to_string() const { return fmt::format("OP_END_IF"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_elif
{
    std::ptrdiff_t jump = -1;

    std::string to_string() const { return fmt::format(PRINT_JUMP, "OP_ELIF", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_else
{
    std::ptrdiff_t jump = -1;

    std::string to_string() const { return fmt::format(PRINT_JUMP, "OP_ELSE", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_while
{
    std::string to_string() const { return fmt::format("OP_WHILE"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_end_while
{
    std::ptrdiff_t jump = -1;

    std::string to_string() const { return fmt::format(PRINT_JUMP, "OP_END_WHILE", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_break
{
    std::ptrdiff_t jump = -1;

    std::string to_string() const { return fmt::format(PRINT_JUMP, "OP_BREAK", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_continue
{
    std::ptrdiff_t jump = -1;

    std::string to_string() const { return fmt::format(PRINT_JUMP, "OP_CONTINUE", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_do
{
    std::ptrdiff_t jump = -1;

    std::string to_string() const { return fmt::format(PRINT_JUMP_IF_FALSE, "OP_DO", jump); }
    void apply(anzu::stack_frame& frame) const;
};

class op
{
    using op_type = std::variant<
        op_store,
        op_dump,
        op_pop,
        op_push_const,
        op_push_var,
        op_add,
        op_sub,
        op_mul,
        op_div,
        op_mod,
        op_dup,
        op_print_frame,
        op_eq,
        op_ne,
        op_lt,
        op_le,
        op_gt,
        op_ge,
        op_or,
        op_and,
        op_input,

        // Control Flow
        op_if,
        op_end_if,
        op_elif,
        op_else,
        op_while,
        op_end_while,
        op_break,
        op_continue,
        op_do
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

    inline void apply(anzu::stack_frame& frame) const {
        return std::visit([&](auto&& o) { o.apply(frame); }, d_type);
    }
};

}