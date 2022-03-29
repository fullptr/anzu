#pragma once
#include <format>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "type.hpp"
#include "utility/print.hpp"

namespace anzu {

struct block;

struct object
{
    std::vector<block> data;
    anzu::type_name    type;
};

struct pointer
{
    std::size_t ptr;
    std::size_t size;
};

using block_int   = int;
using block_float = double;
using block_bool  = bool;
using block_str   = std::string;
using block_list  = std::shared_ptr<std::vector<block>>;
using block_ptr   = pointer;
using block_null  = std::monostate;

auto to_string(const block& blk) -> std::string;
auto to_string(const object& object) -> std::string;

struct block : std::variant<
    block_int,
    block_float,
    block_bool,
    block_str,
    block_list,
    block_ptr,
    block_null
>
{
    using variant::variant;
};

auto make_int(block_int val) -> object;
auto make_bool(block_bool val) -> object;
auto make_str(const block_str& val) -> object;
auto make_null() -> object;

// Should be elsewhere
auto format_special_chars(const std::string& str) -> std::string;

}

template <> struct std::formatter<anzu::block> : std::formatter<std::string> {
    auto format(const anzu::block& blk, auto& ctx) {
        return std::formatter<std::string>::format(to_string(blk), ctx);
    }
};

template <> struct std::formatter<anzu::object> : std::formatter<std::string> {
    auto format(const anzu::object& obj, auto& ctx) {
        return std::formatter<std::string>::format(to_string(obj), ctx);
    }
};