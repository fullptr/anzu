#include "program.hpp"
#include "object.hpp"

#include <string>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

constexpr auto FORMAT2 = std::string_view{"{:<30} {}"};
constexpr auto FORMAT3 = std::string_view{"{:<30} {:<20} {}"};

}

auto to_string(const op& op_code) -> std::string
{
    return std::visit(overloaded {
        [&](const op_push_const& op) {
            return std::format("OP_PUSH_CONST({})", op.value.to_repr());
        },
        [&](const op_push_var& op) {
            return std::format("OP_PUSH_VAR({})", op.name);
        },
        [&](const op_pop& op) {
            return std::string{"OP_POP"};
        },
        [&](const op_copy_index& op) {
            return std::format("OP_COPY_INDEX({})", op.index);
        },
        [&](const op_store& op) {
            return std::format("OP_STORE({})", op.name);
        },
        [&](const op_if& op) {
            return std::string{"OP_IF"};
        },
        [&](const op_if_end& op) {
            return std::string{"OP_END_IF"};
        },
        [&](const op_else& op) {
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, "OP_ELSE", jump_str);
        },
        [&](const op_while& op) {
            return std::string{"OP_WHILE"};
        },
        [&](const op_while_end& op) {
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, "OP_END_WHILE", jump_str);
        },
        [&](const op_for& op) {
            return std::string{"OP_FOR"};
        },
        [&](const op_for_end& op) {
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, "OP_END_FOR", jump_str);
        },
        [&](const op_break& op) {
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, "OP_BREAK", jump_str);
        },
        [&](const op_continue& op) {
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, "OP_CONTINUE", jump_str);
        },
        [&](const op_jump_if_false& op) {
            const auto jump_str = std::format("JUMP -> {} IF FALSE", op.jump);
            return std::format(FORMAT2, "OP_JUMP_IF_FALSE", jump_str);
        },
        [&](const op_function& op) {
            const auto func_str = std::format("OP_FUNCTION({})", op.name);
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, func_str, jump_str);
        },
        [&](const op_function_end& op) {
            return std::string{"OP_FUNCTION_END"};
        },
        [&](const op_return& op) {
            return std::string{"OP_RETURN"};
        },
        [&](const op_function_call& op) {
            const auto func_str = std::format("OP_FUNCTION_CALL({})", op.name);
            const auto jump_str = std::format("JUMP -> {}", op.ptr);
            return std::format(FORMAT2, func_str, jump_str);
        },
        [&](const op_builtin_call& op) {
            return std::format("OP_BUILTIN_CALL({})", op.name);
        },
        [&](const op_add& op) {
            return std::string{"OP_ADD"};
        },
        [&](const op_sub& op) {
            return std::string{"OP_SUB"};
        },
        [&](const op_mul& op) {
            return std::string{"OP_MUL"};
        },
        [&](const op_div& op) {
            return std::string{"OP_DIV"};
        },
        [&](const op_mod& op) {
            return std::string{"OP_MOD"};
        },
        [&](const op_eq& op) {
            return std::string{"OP_EQ"};
        },
        [&](const op_ne& op) {
            return std::string{"OP_NE"};
        },
        [&](const op_lt& op) {
            return std::string{"OP_LT"};
        },
        [&](const op_le& op) {
            return std::string{"OP_LE"};
        },
        [&](const op_gt& op) {
            return std::string{"OP_GT"};
        },
        [&](const op_ge& op) {
            return std::string{"OP_GE"};
        },
        [&](const op_or& op) {
            return std::string{"OP_OR"};
        },
        [&](const op_and& op) {
            return std::string{"OP_AND"};
        },
        [&](const op_debug& op) {
            return std::string{"OP_DEBUG"};
        }
    }, op_code);
}

auto print_program(const anzu::program& program) -> void
{
    int lineno = 0;
    for (const auto& op : program) {
        anzu::print("{:>4} - {}\n", lineno++, anzu::to_string(op));
    }
}

}