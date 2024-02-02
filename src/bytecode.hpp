#pragma once
#include "allocator.hpp"

#include <cstdint>
#include <vector>

namespace anzu {

enum class op : std::uint8_t
{
    push_i32,
    push_i64,
    push_u64,
    push_f64,
    push_char,
    push_bool,
    push_null,

    push_string_literal,
    push_call_frame,
    push_ptr,
    push_ptr_rel,
    
    load,
    save,
    pop,
    alloc_span,
    dealloc_span,
    alloc_ptr,
    dealloc_ptr,
    jump,
    jump_if_false,
    call,
    builtin_call,
    ret,
    assert,

    char_eq,
    char_ne,

    i32_add,
    i32_sub,
    i32_mul,
    i32_div,
    i32_mod,
    i32_eq,
    i32_ne,
    i32_lt,
    i32_le,
    i32_gt,
    i32_ge,

    i64_add,
    i64_sub,
    i64_mul,
    i64_div,
    i64_mod,
    i64_eq,
    i64_ne,
    i64_lt,
    i64_le,
    i64_gt,
    i64_ge,

    u64_add,
    u64_sub,
    u64_mul,
    u64_div,
    u64_mod,
    u64_eq,
    u64_ne,
    u64_lt,
    u64_le,
    u64_gt,
    u64_ge,

    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_eq,
    f64_ne,
    f64_lt,
    f64_le,
    f64_gt,
    f64_ge,

    bool_or,
    bool_and,
    bool_eq,
    bool_ne,
    bool_not,

    i32_neg,
    i64_neg,
    f64_neg,

    print_null,
    print_bool,
    print_char,
    print_i32,
    print_i64,
    print_u64,
    print_f64,
    print_char_span,
};

using bytecode = std::vector<std::byte>;

struct bytecode_context
{
    std::size_t prog_ptr = 0;
    std::size_t base_ptr = 0;

    std::vector<std::byte> stack;
    std::vector<std::byte> heap;
    std::vector<std::byte> rom;

    memory_allocator allocator;

    bytecode_context() : allocator{heap} {}
};

struct bytecode_program
{
    std::vector<std::byte> code;
    std::vector<std::byte> rom;
};

auto run_program(const bytecode_program& prog) -> void;
auto run_program_debug(const bytecode_program& prog) -> void;

auto print_program(const bytecode_program& prog) -> void;

}