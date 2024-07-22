#include "bytecode.hpp"
#include "functions.hpp"

#include <print>
#include <cstddef>

namespace anzu {
namespace {

template <typename T>
requires std::integral<T> || std::floating_point<T> || std::is_same_v<T, std::byte*> || std::is_same_v<T, op>
auto read_at(const std::vector<std::byte>& code, std::size_t& ptr) -> T
{
    auto ret = T{};
    std::memcpy(&ret, &code[ptr], sizeof(T));
    ptr += sizeof(T);
    return ret;
}

}

auto print_op(const bytecode_program& prog, std::size_t ptr) -> std::size_t
{
    std::print("[{:>3}] ", ptr);
    const auto op_code = read_at<op>(prog.code, ptr);
    switch (op_code) {
        case op::push_i32: {
            const auto value = read_at<std::int32_t>(prog.code, ptr);
            std::print("PUSH_I32: {}\n", value);
        } break;
        case op::push_i64: {
            const auto value = read_at<std::int64_t>(prog.code, ptr);
            std::print("PUSH_I64: {}\n", value);
        } break;
        case op::push_u64: {
            const auto value = read_at<std::uint64_t>(prog.code, ptr);
            std::print("PUSH_U64: {}\n", value);
        } break;
        case op::push_f64: {
            const auto value = read_at<double>(prog.code, ptr);
            std::print("PUSH_F64: {}\n", value);
        } break;
        case op::push_char: {
            const auto value = read_at<char>(prog.code, ptr);
            std::print("PUSH_CHAR: {}\n", value);
        } break;
        case op::push_bool: {
            const auto value = read_at<bool>(prog.code, ptr);
            std::print("PUSH_BOOL: {}\n", value);
        } break;
        case op::push_null: {
            std::print("PUSH_NULL\n");
        } break;
        case op::push_string_literal: {
            const auto index = read_at<std::uint64_t>(prog.code, ptr);
            const auto size = read_at<std::uint64_t>(prog.code, ptr);
            const auto data = &prog.rom[index];
            const auto m = std::string_view(data, size);
            std::print("PUSH_STRING_LITERAL: '{}'\n", m);
        } break;
        case op::push_ptr_global: {
            const auto offset = read_at<std::uint64_t>(prog.code, ptr);
            std::print("PUSH_PTR_GLOBAL: {}\n", offset);
        } break;
        case op::push_ptr_local: {
            const auto offset = read_at<std::uint64_t>(prog.code, ptr);
            std::print("PUSH_PTR_LOCAL: base_ptr + {}\n", offset);
        } break;
        case op::new_arena: {
            std::print("NEW_ARENA\n");
        } break;
        case op::delete_arena: {
            std::print("DELETE_ARENA\n");
        } break;
        case op::allocate: {
            const auto size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("ALLOCATE: size={}\n", size);
        } break;
        case op::load: {
            const auto size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("LOAD: {}\n", size);
        } break;
        case op::save: {
            const auto size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("SAVE: {}\n", size);
        } break;
        case op::pop: {
            const auto size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("POP: {}\n", size);
        } break;
        case op::alloc_span: {
            const auto type_size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("ALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op::dealloc_span: {
            const auto type_size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("DEALLOC_SPAN: type_size={}\n", type_size);
        } break;
        case op::alloc_ptr: {
            const auto type_size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("ALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op::dealloc_ptr: {
            const auto type_size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("DEALLOC_PTR: type_size={}\n", type_size);
        } break;
        case op::jump: {
            const auto jump = read_at<std::uint64_t>(prog.code, ptr);
            std::print("JUMP: jump={}\n", jump);
        } break;
        case op::jump_if_false: {
            const auto jump = read_at<std::uint64_t>(prog.code, ptr);
            std::print("JUMP_IF_FALSE: jump={}\n", jump);
        } break;
        case op::ret: {
            const auto type_size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("RETURN: type_size={}\n", type_size);
        } break;
        case op::call: {
            const auto args_size = read_at<std::uint64_t>(prog.code, ptr);
            std::print("CALL: args_size={}\n", args_size);
        } break;
        case op::builtin_call: {
            const auto id = read_at<std::uint64_t>(prog.code, ptr);
            const auto& b = get_builtin(id);
            std::print("BUILTIN_CALL: {}({}) -> {}\n",
                  b.name, format_comma_separated(b.args), b.return_type);
        } break;
        case op::assert: {
            const auto index = read_at<std::uint64_t>(prog.code, ptr);
            const auto size = read_at<std::uint64_t>(prog.code, ptr);
            const auto data = &prog.rom[index];
            std::print("ASSERT: msg={}\n", std::string_view{data, size});
        } break;
        case op::char_eq: { std::print("CHAR_EQ\n"); } break;
        case op::char_ne: { std::print("CHAR_NE\n"); } break;
        case op::i32_add: { std::print("I32_ADD\n"); } break;
        case op::i32_sub: { std::print("I32_SUB\n"); } break;
        case op::i32_mul: { std::print("I32_MUL\n"); } break;
        case op::i32_div: { std::print("I32_DIV\n"); } break;
        case op::i32_mod: { std::print("I32_MOD\n"); } break;
        case op::i32_eq:  { std::print("I32_EQ\n"); } break;
        case op::i32_ne:  { std::print("I32_NE\n"); } break;
        case op::i32_lt:  { std::print("I32_LT\n"); } break;
        case op::i32_le:  { std::print("I32_LE\n"); } break;
        case op::i32_gt:  { std::print("I32_GT\n"); } break;
        case op::i32_ge:  { std::print("I32_GE\n"); } break;
        case op::i64_add: { std::print("I64_ADD\n"); } break;
        case op::i64_sub: { std::print("I64_SUB\n"); } break;
        case op::i64_mul: { std::print("I64_MUL\n"); } break;
        case op::i64_div: { std::print("I64_DIV\n"); } break;
        case op::i64_mod: { std::print("I64_MOD\n"); } break;
        case op::i64_eq:  { std::print("I64_EQ\n"); } break;
        case op::i64_ne:  { std::print("I64_NE\n"); } break;
        case op::i64_lt:  { std::print("I64_LT\n"); } break;
        case op::i64_le:  { std::print("I64_LE\n"); } break;
        case op::i64_gt:  { std::print("I64_GT\n"); } break;
        case op::i64_ge:  { std::print("I64_GE\n"); } break;
        case op::u64_add: { std::print("U64_ADD\n"); } break;
        case op::u64_sub: { std::print("U64_SUB\n"); } break;
        case op::u64_mul: { std::print("U64_MUL\n"); } break;
        case op::u64_div: { std::print("U64_DIV\n"); } break;
        case op::u64_mod: { std::print("U64_MOD\n"); } break;
        case op::u64_eq:  { std::print("U64_EQ\n"); } break;
        case op::u64_ne:  { std::print("U64_NE\n"); } break;
        case op::u64_lt:  { std::print("U64_LT\n"); } break;
        case op::u64_le:  { std::print("U64_LE\n"); } break;
        case op::u64_gt:  { std::print("U64_GT\n"); } break;
        case op::u64_ge:  { std::print("U64_GE\n"); } break;
        case op::f64_add: { std::print("F64_ADD\n"); } break;
        case op::f64_sub: { std::print("F64_SUB\n"); } break;
        case op::f64_mul: { std::print("F64_MUL\n"); } break;
        case op::f64_div: { std::print("F64_DIV\n"); } break;
        case op::f64_eq:  { std::print("F64_EQ\n"); } break;
        case op::f64_ne:  { std::print("F64_NE\n"); } break;
        case op::f64_lt:  { std::print("F64_LT\n"); } break;
        case op::f64_le:  { std::print("F64_LE\n"); } break;
        case op::f64_gt:  { std::print("F64_GT\n"); } break;
        case op::f64_ge:  { std::print("F64_GE\n"); } break;
        case op::bool_and: { std::print("BOOL_AND\n"); } break;
        case op::bool_or:  { std::print("BOOL_OR\n"); } break;
        case op::bool_eq:  { std::print("BOOL_EQ\n"); } break;
        case op::bool_ne:  { std::print("BOOL_NE\n"); } break;
        case op::bool_not: { std::print("BOOL_NOT\n"); } break;
        case op::i32_neg: { std::print("I32_NEG\n"); } break;
        case op::i64_neg: { std::print("I64_NEG\n"); } break;
        case op::f64_neg: { std::print("F64_NEG\n"); } break;
        case op::print_null: { std::print("PRINT_NULL\n"); } break;
        case op::print_bool: { std::print("PRINT_BOOL\n"); } break;
        case op::print_char: { std::print("PRINT_CHAR\n"); } break;
        case op::print_i32: { std::print("PRINT_I32\n"); } break;
        case op::print_i64: { std::print("PRINT_I64\n"); } break;
        case op::print_u64: { std::print("PRINT_U64\n"); } break;
        case op::print_f64: { std::print("PRINT_F64\n"); } break;
        case op::print_char_span: { std::print("PRINT_STRING_LITERAL\n");
        } break;
        default: {
            std::print("UNKNOWN\n");
            return 0;
        } break;
    }
    return ptr;
}

auto print_program(const bytecode_program& prog) -> void
{
    auto ptr = std::size_t{0};
    while (ptr < prog.code.size()) {
        ptr = print_op(prog, ptr);
    }
}

}