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
        [&](op_push_global_addr op) {
            return std::format("PUSH_GLOBAL_ADDR({})", op.position);
        },
        [&](op_push_local_addr op) {
            return std::format("PUSH_LOCAL_ADDR(+{})", op.offset);
        },
        [&](op_modify_ptr) {
            return std::string{"MODIFY_PTR"};
        },
        [&](op_load op) {
            return std::format("LOAD({})", op.size);
        },
        [&](op_save op) {
            return std::format("SAVE({})", op.size);
        },
        [&](op_pop op) {
            return std::format("POP({})", op.size);
        },
        [&](op_allocate op) {
            return std::format("ALLOCATE({})", op.size);
        },
        [&](op_deallocate op) {
            return std::format("DEALLOCATE({})", op.size);
        },
        [&](op_jump op) {
            return std::format(FORMAT2, "JUMP_RELATIVE", op.jump);
        },
        [&](op_jump_if_false op) {
            return std::format(FORMAT2, "JUMP_RELATIVE_IF_FALSE", op.jump);
        },
        [&](const op_function& op) {
            const auto func_str = std::format("FUNCTION({})", op.name);
            const auto jump_str = std::format("JUMP -> {}", op.jump);
            return std::format(FORMAT2, func_str, jump_str);
        },
        [&](op_return op) {
            return std::format("RETURN({})", op.size);
        },
        [&](const op_function_call& op) {
            const auto func_str = std::format("FUNCTION_CALL({})", op.name);
            const auto jump_str = std::format("JUMP -> {}", op.ptr);
            return std::format(FORMAT2, func_str, jump_str);
        },
        [&](const op_builtin_call& op) {
            return std::format("BUILTIN_CALL({})", op.name);
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