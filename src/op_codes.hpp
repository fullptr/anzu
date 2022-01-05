#pragma once
#include "stack_frame.hpp"

#include <fmt/format.h>
#include <variant>

namespace anzu {
namespace op {

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

struct op_if
{
    void print() const { fmt::print("OP_BEGIN_IF\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_do // TODO: Improve name
{
    int jump;

    void print() const { fmt::print("OP_DO({})\n", jump); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_else
{
    int jump;

    void print() const { fmt::print("OP_ELSE_IF({})\n", jump); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_end
{
    void print() const { fmt::print("OP_END_IF\n"); }
    int apply(anzu::stack_frame& frame) const;
};

struct op_equals
{
    void print() const { fmt::print("OP_EQUALS\n"); }
    int apply(anzu::stack_frame& frame) const;
};

}

using opcode = std::variant<
    op::op_dump,
    op::op_pop,
    op::op_push_const,
    op::op_store_const,
    op::op_push_var,
    op::op_store_var,
    op::op_add,
    op::op_sub,
    op::op_dup,
    op::op_print_frame,
    op::op_if,
    op::op_do,
    op::op_else,
    op::op_end,
    op::op_equals
>;

}