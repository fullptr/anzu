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
    anzu::type         type;
};

using block_int  = int;
using block_bool = bool;
using block_str  = std::string;
using block_list = std::shared_ptr<std::vector<block>>;
using block_null = std::monostate;

auto to_string(const block& blk) -> std::string;
auto to_string(const object& object) -> std::string;

struct block : std::variant<
    block_int,
    block_bool,
    block_str,
    block_list,
    block_null
>
{
    using variant::variant;
};

inline auto null_object() -> object
{
    return { .data = { block{block_null()} }, .type = null_type() };
}

auto format_special_chars(const std::string& str) -> std::string;

inline auto make_int_object(int val) -> object
{
    return object{ .data = { block{val} }, .type = int_type() };
}

}

template <> struct std::formatter<anzu::block> : std::formatter<std::string> {
    char fmt = 's';

    constexpr auto parse(std::format_parse_context& ctx) -> decltype(ctx.begin()) {
        auto it = ctx.begin(), end = ctx.end();
        if (it != end && (*it == 's' || *it == 'r')) fmt = *it++;
        if (it != end && *it != '}') throw format_error("invalid format");
        return it;
    }

    auto format(const anzu::block& blk, auto& ctx) {
        return std::formatter<std::string>::format(std::format("{}", to_string(blk)), ctx);
    }
};