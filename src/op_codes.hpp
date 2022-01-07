#pragma once
#include "stack_frame.hpp"

#include <fmt/format.h>
#include <variant>

namespace anzu {

struct op_store
{
    std::string name;

    void print() const { fmt::print("OP_STORE({})\n", name); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_dump
{
    void print() const { fmt::print("OP_DUMP\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_pop
{
    void print() const { fmt::print("OP_POP\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_push_const
{
    anzu::stack_frame::type value;

    void print() const {
        fmt::print("OP_PUSH_CONST(");
        anzu::print_value(value);
        fmt::print(")\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_push_var
{
    std::string name;

    void print() const { fmt::print("OP_PUSH_VAR({})\n", name); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_add
{
    void print() const { fmt::print("OP_ADD\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_sub
{
    void print() const { fmt::print("OP_SUB\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_mul
{
    void print() const { fmt::print("OP_MUL\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_div
{
    void print() const { fmt::print("OP_DIV\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_mod
{
    void print() const { fmt::print("OP_MOD\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_dup
{
    void print() const { fmt::print("OP_DUP\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_print_frame
{
    void print() const { fmt::print("OP_PRINT_FRAME\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_do
{
    int jump;

    void print() const { fmt::print("OP_DO({})\n", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_while
{
    void print() const { fmt::print("OP_WHILE\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_if
{
    void print() const { fmt::print("OP_IF\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_else
{
    int jump;

    void print() const { fmt::print("OP_ELSE({})\n", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_end
{
    int jump;

    void print() const { fmt::print("OP_END({})\n", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_eq
{
    void print() const { fmt::print("OP_EQ\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_ne
{
    void print() const { fmt::print("OP_NE\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_lt
{
    void print() const { fmt::print("OP_LT\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_le
{
    void print() const { fmt::print("OP_LE\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_gt
{
    void print() const { fmt::print("OP_GT\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_ge
{
    void print() const { fmt::print("OP_GE\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_or
{
    void print() const { fmt::print("OP_OR\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_and
{
    void print() const { fmt::print("OP_AND\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_input
{
    void print() const { fmt::print("OP_INPUT\n"); }
    void apply(anzu::stack_frame& frame) const;
};

using op = std::variant<
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
    op_while,
    op_if,
    op_do,
    op_else,
    op_end,
    op_eq,
    op_ne,
    op_lt,
    op_le,
    op_gt,
    op_ge,
    op_or,
    op_and,
    op_input
>;

}