#include "program.hpp"
#include "object.hpp"
#include "utility/overloaded.hpp"

#include <string>

namespace anzu {
namespace {

constexpr auto FORMAT2 = std::string_view{"{:<30} {}"};
constexpr auto FORMAT3 = std::string_view{"{:<30} {:<20} {}"};

}

auto to_string(const op& op_code) -> std::string
{
    return std::visit(overloaded {
        [&](const op_load_literal& op) {
            return std::format("OP_LOAD_LITERAL({})", format_comma_separated(op.value));
        },
        [&](const op_push_global_addr& op) {
            return std::format("OP_PUSH_GLOBAL_ADDR({}, {})", op.position, op.size);
        },
        [&](const op_push_local_addr& op) {
            return std::format("OP_PUSH_LOCAL_ADDR(+{}, {})", op.offset, op.size);
        },
        [&](const op_load&) {
            return std::string{"OP_LOAD"};
        },
        [&](const op_save& op) {
            return std::string{"OP_SAVE"};
        },
        [&](const op_pop& op) {
            return std::format("OP_POP({})", op.size);
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
        [&](const op_loop_begin& op) {
            return std::string{"OP_LOOP_BEGIN"};
        },
        [&](const op_loop_end& op) {
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, "OP_LOOP_END", jump_str);
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
        [&](const op_builtin_mem_op& op) {
            return std::format("OP_BUILTIN_MEM_OP({})", op.name);
        },
        [&](const op_build_list& op) {
            return std::format("OP_BUILD_LIST({})", op.size);
        }
    }, op_code);
}

auto print_program(const anzu::program& program) -> void
{
    int lineno = 0;
    for (const auto& op : program) {
        anzu::print("{:>4} - {}\n", lineno++, op);
    }
}

}