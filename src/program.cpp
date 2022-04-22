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
        [&](const op_load_bytes& op) {
            return std::format("LOAD_BYTES({})", format_comma_separated(op.bytes));
        },
        [&](const op_push_global_addr& op) {
            return std::format("PUSH_GLOBAL_ADDR({})", op.position);
        },
        [&](const op_push_local_addr& op) {
            return std::format("PUSH_LOCAL_ADDR(+{})", op.offset);
        },
        [&](const op_modify_ptr& op) {
            return std::string{"MODIFY_PTR"};
        },
        [&](const op_load& op) {
            return std::format("LOAD({})", op.size);
        },
        [&](const op_save& op) {
            return std::format("SAVE({})", op.size);
        },
        [&](const op_pop& op) {
            return std::format("POP({})", op.size);
        },
        [&](op_jump_relative op) {
            return std::format(FORMAT2, "JUMP_RELATIVE", op.jump);
        },
        [&](const op_jump_if_false& op) {
            const auto jump_str = std::format("JUMP -> {} IF FALSE", op.jump);
            return std::format(FORMAT2, "JUMP_IF_FALSE", jump_str);
        },
        [&](const op_function& op) {
            const auto func_str = std::format("FUNCTION({})", op.name);
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, func_str, jump_str);
        },
        [&](const op_return& op) {
            return std::string{"RETURN"};
        },
        [&](const op_function_call& op) {
            const auto func_str = std::format("FUNCTION_CALL({})", op.name);
            const auto jump_str = std::format("JUMP -> {}", op.ptr);
            return std::format(FORMAT2, func_str, jump_str);
        },
        [&](const op_builtin_call& op) {
            return std::format("BUILTIN_CALL({})", op.name);
        },
        [&](const op_builtin_mem_op& op) {
            return std::format("BUILTIN_MEM_OP({})", op.name);
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