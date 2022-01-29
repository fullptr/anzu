#pragma once
#include "stack_frame.hpp"
#include "functions.hpp"

#include <variant>
#include <format>

namespace anzu {

constexpr auto FORMAT2 = std::string_view{"{:<30} {}"};
constexpr auto FORMAT3 = std::string_view{"{:<30} {:<20} {}"};

// Stack Manipulation

struct op_push_const
{
    anzu::object value;

    std::string to_string() const { return std::format("OP_PUSH_CONST({})", value.to_repr()); }
    void apply(anzu::context& ctx) const;
};

struct op_push_var
{
    std::string name;

    std::string to_string() const { return std::format("OP_PUSH_VAR({})", name); }
    void apply(anzu::context& ctx) const;
};

struct op_pop
{
    std::string to_string() const { return "OP_POP"; }
    void apply(anzu::context& ctx) const;
};

// 0 == OP_DUP, 1 == OP_OVER, ...
struct op_copy_index
{
    int index;

    std::string to_string() const { return std::format("OP_COPY_INDEX({})", index); }
    void apply(anzu::context& ctx) const;
};

// Store Manipulation

struct op_store
{
    std::string name;

    std::string to_string() const { return std::format("OP_STORE({})", name); }
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

struct op_else
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = std::format("JUMP -> {}", jump);
        return std::format(FORMAT2, "OP_ELSE", jump_str);
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
        const auto jump_str = std::format("JUMP -> {}", jump);
        return std::format(FORMAT2, "OP_END_WHILE", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_for
{
    std::string to_string() const { return "OP_FOR"; }
    void apply(anzu::context& ctx) const;
};

struct op_for_end
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = std::format("JUMP -> {}", jump);
        return std::format(FORMAT2, "OP_END_FOR", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_break
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = std::format("JUMP -> {}", jump);
        return std::format(FORMAT2, "OP_BREAK", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_continue
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = std::format("JUMP -> {}", jump);
        return std::format(FORMAT2, "OP_CONTINUE", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_jump_if_false
{
    std::intptr_t jump = -1;

    std::string to_string() const
    {
        const auto jump_str = std::format("JUMP -> {} IF FALSE", jump);
        return std::format(FORMAT2, "OP_JUMP_IF_FALSE", jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_function_call
{
    std::string   name;
    std::intptr_t ptr;
    std::vector<std::string> arg_names;

    std::string to_string() const { return std::format("OP_FUNCTION_CALL({})", name); }
    void apply(anzu::context& ctx) const;
};

struct op_builtin_call
{
    std::string name;
    anzu::builtin_function func;

    std::string to_string() const { return std::format("OP_BUILTIN_CALL({})", name); }
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

struct op_function
{
    std::string              name;
    std::vector<std::string> arg_names;
    std::intptr_t            jump;
    std::string to_string() const
    {
        const auto func_str = std::format("OP_FUNCTION({})", name);
        const auto jump_str = std::format("JUMP -> {}", jump);
        return std::format(FORMAT2, func_str, jump_str);
    }
    void apply(anzu::context& ctx) const;
};

struct op_function_end
{
    std::string to_string() const { return "OP_FUNCTION_END"; }
    void apply(anzu::context& ctx) const;
};

struct op_return
{
    std::string to_string() const { return "OP_RETURN"; }
    void apply(anzu::context& ctx) const;
};

class op
{
    using op_type = std::variant<
        op_push_const,
        op_push_var,
        op_pop,
        op_copy_index,

        // Store Manipulation
        op_store,

        // Control Flow
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

        op_function,
        op_function_end,
        op_return,
        op_function_call,
        op_builtin_call
    >;

    op_type d_type;

public:
    template <typename Op>
    op(const Op& op_type) : d_type(op_type) {}

    template <typename Op>
    Op* get_if() { return std::get_if<Op>(&d_type); }

    template <typename Op>
    Op& as() {
        auto* ret = get_if<Op>();
        if (ret == nullptr) {
            anzu::print("parser error: op code did not contain the expected type\n");
            std::exit(1);
        }
        return *ret;
    }

    inline std::string to_string() const {
        return std::visit([](auto&& o) { return o.to_string(); }, d_type);
    }

    inline void apply(anzu::context& ctx) const {
        return std::visit([&](auto&& o) { o.apply(ctx); }, d_type);
    }
};

}

template <> struct std::formatter<anzu::op> : std::formatter<std::string> {
    auto format(const anzu::op& op, auto& ctx) {
        return std::formatter<std::string>::format(
            std::format("{}", op.to_string()), ctx
        );
    }
};