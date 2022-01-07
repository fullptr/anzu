#pragma once
#include "stack_frame.hpp"

#include <fmt/format.h>
#include <variant>

namespace anzu {

constexpr auto PRINT_JUMP          = std::string_view{"{:<25} JUMP -> {}\n"};
constexpr auto PRINT_JUMP_IF_FALSE = std::string_view{"{:<25} JUMP -> {} (IF FALSE)\n"};

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
    anzu::object value;

    void print() const { fmt::print("OP_PUSH_CONST({})\n", value); }
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

struct op_if
{
    void print() const { fmt::print("OP_IF\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_end_if
{
    void print() const { fmt::print("OP_END_IF\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_elif
{
    std::ptrdiff_t jump = -1;

    void print() const { fmt::print(PRINT_JUMP, "OP_ELIF", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_else
{
    std::ptrdiff_t jump = -1;

    void print() const { fmt::print(PRINT_JUMP, "OP_ELSE", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_while
{
    void print() const { fmt::print("OP_WHILE\n"); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_end_while
{
    std::ptrdiff_t jump = -1;

    void print() const { fmt::print(PRINT_JUMP, "OP_END_WHILE", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_break
{
    std::ptrdiff_t jump = -1;

    void print() const { fmt::print(PRINT_JUMP, "OP_BREAK", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_continue
{
    std::ptrdiff_t jump = -1;

    void print() const { fmt::print(PRINT_JUMP, "OP_CONTINUE", jump); }
    void apply(anzu::stack_frame& frame) const;
};

struct op_do
{
    std::ptrdiff_t jump = -1;

    void print() const { fmt::print(PRINT_JUMP_IF_FALSE, "OP_DO", jump); }
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

    template <typename Visitor>
    decltype(auto) visit(Visitor&& f) const {
        return std::visit(std::forward<Visitor>(f), d_type);
    }

public:
    template <typename Op>
    op(const Op& op_type) : d_type(op_type) {}

    template <typename Op>
    Op* get_if() { return std::get_if<Op>(&d_type); }

    void print() const {
        visit([](auto&& o) { o.print(); });
    }

    void apply(anzu::stack_frame& frame) const {
        visit([&](auto&& o) { o.apply(frame); });
    }
};

}