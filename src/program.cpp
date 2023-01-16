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
        [&](op_i32_add) { return std::string{"I32_ADD"}; },
        [&](op_i32_sub) { return std::string{"I32_SUB"}; },
        [&](op_i32_mul) { return std::string{"I32_MUL"}; },
        [&](op_i32_div) { return std::string{"I32_DIV"}; },
        [&](op_i64_add) { return std::string{"I64_ADD"}; },
        [&](op_i64_sub) { return std::string{"I64_SUB"}; },
        [&](op_i64_mul) { return std::string{"I64_MUL"}; },
        [&](op_i64_div) { return std::string{"I64_DIV"}; },
        [&](op_u64_add) { return std::string{"U64_ADD"}; },
        [&](op_u64_sub) { return std::string{"U64_SUB"}; },
        [&](op_u64_mul) { return std::string{"U64_MUL"}; },
        [&](op_u64_div) { return std::string{"U64_DIV"}; },
        [&](op_f64_add) { return std::string{"F64_ADD"}; },
        [&](op_f64_sub) { return std::string{"F64_SUB"}; },
        [&](op_f64_mul) { return std::string{"F64_MUL"}; },
        [&](op_f64_div) { return std::string{"F64_DIV"}; },
        [&](op_bool_and) { return std::string{"BOOL_AND"}; },
        [&](op_bool_or) { return std::string{"BOOL_OR"}; },
        [&](op_bool_eq) { return std::string{"BOOL_EQ"}; },
        [&](op_bool_ne) { return std::string{"BOOL_NE"}; },
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
            return std::format("ALLOCATE({})", op.type_size);
        },
        [&](op_deallocate op) {
            return std::string{"DEALLOCATE"};
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
        },
        [](const op_debug& op) {
            return std::format("DEBUG({})", op.message);
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