#pragma once
#include <cstdint>
#include <string>
#include <vector>

namespace anzu {

// Maps a bytecode offset within a function to a source line number.
struct source_location
{
    std::size_t offset;  // byte offset from function code start
    std::size_t line;    // 1-based source line number
};

// Debug info for a single variable: name, type, stack location, and when it becomes live.
struct debug_variable
{
    std::string name;
    std::string type_str;
    std::size_t location;   // byte offset from base_ptr (locals) or from stack[0] (globals)
    std::size_t size;       // size in bytes
    bool        is_global;
    std::size_t live_from;  // bytecode offset when this variable was declared
};

struct bytecode_function
{
    std::string                  name;
    std::size_t                  id;
    std::vector<std::byte>       code;
    std::vector<source_location> source_map;  // offset → source line
    std::vector<debug_variable>  dbg_vars;    // variable debug info
};

struct bytecode_program
{
    std::vector<bytecode_function> functions;
    std::string                    rom;
};

auto print_program(const bytecode_program& prog) -> void;
auto print_op(std::string_view rom, const std::byte* start, const std::byte* ptr) -> const std::byte*;

enum class op : std::uint8_t
{
    end_program,
    
    push_i32,
    push_i64,
    push_u64,
    push_f64,
    push_char,
    push_bool,
    push_null,
    push_nullptr,

    push_string_literal,
    push_ptr_global,
    push_ptr_local,
    push_val_global,
    push_val_local,
    push_function_ptr,

    nth_element_ptr,
    nth_element_val,
    span_ptr_to_len,
    push_subspan,

    arena_new,
    arena_delete,
    arena_alloc,
    arena_alloc_array,
    arena_realloc_array,
    arena_size,
    
    load,
    save,
    push,
    pop,
    memcpy,
    memcmp,
    jump,
    jump_if_true,
    jump_if_false,
    call_static,
    call_ptr,
    ret,
    assert,

    read_file,

    null_to_i64,
    bool_to_i64,
    char_to_i64,
    i32_to_i64,
    u64_to_i64,
    f64_to_i64,

    null_to_u64,
    bool_to_u64,
    char_to_u64,
    i32_to_u64,
    i64_to_u64,
    f64_to_u64,

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
    print_ptr,
};

}