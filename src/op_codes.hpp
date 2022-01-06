#pragma once
#include "stack_frame.hpp"

#include <fmt/format.h>
#include <variant>

namespace anzu {

struct op_dump
{
    void print() const { fmt::print("OP_DUMP\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_pop
{
    void print() const { fmt::print("OP_POP\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_push_const
{
    anzu::stack_frame::type value;

    void print() const {
        fmt::print("OP_PUSH_CONST(");
        anzu::print_value(value);
        fmt::print(")\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_store_const
{
    std::string name;
    anzu::stack_frame::type value;

    void print() const {
        fmt::print("OP_STORE_CONST({}, ", name);
        anzu::print_value(value);
        fmt::print(")\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_push_var
{
    std::string name;

    void print() const { fmt::print("OP_PUSH_VAR({})\n", name); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_store_var
{
    std::string name;
    std::string source; 

    void print() const { fmt::print("OP_STORE_VAR({}, {})\n", name, source); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_add
{
    void print() const { fmt::print("OP_ADD\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_sub
{
    void print() const { fmt::print("OP_SUB\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_dup
{
    void print() const { fmt::print("OP_DUP\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_print_frame
{
    void print() const { fmt::print("OP_PRINT_FRAME\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_do
{
    int jump;

    void print() const { fmt::print("OP_DO({})\n", jump); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_while
{
    void print() const { fmt::print("OP_WHILE\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_if
{
    void print() const { fmt::print("OP_IF\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_else
{
    int jump;

    void print() const { fmt::print("OP_ELSE({})\n", jump); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_end
{
    int jump;

    void print() const { fmt::print("OP_END({})\n", jump); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_eq
{
    void print() const { fmt::print("OP_EQ\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_ne
{
    void print() const { fmt::print("OP_NE\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_lt
{
    void print() const { fmt::print("OP_LT\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_le
{
    void print() const { fmt::print("OP_LE\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_gt
{
    void print() const { fmt::print("OP_GT\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_ge
{
    void print() const { fmt::print("OP_GE\n"); }
    int apply(anzu::stack_frame& frame) const;
};

using op = std::variant<
    op_dump,
    op_pop,
    op_push_const,
    op_store_const,
    op_push_var,
    op_store_var,
    op_add,
    op_sub,
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
    op_le
>;

}