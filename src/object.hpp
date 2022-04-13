#pragma once
#include <format>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "type.hpp"
#include "utility/print.hpp"

namespace anzu {

// Want these to be equivalent since we want uints available in the runtime but we also want
// to use it as indexes into C++ vectors which use size_t.
static_assert(sizeof(std::uint64_t) == sizeof(std::size_t));

using block_byte  = std::byte;
using block_int   = std::int64_t;
using block_uint  = std::uint64_t;
using block_float = double;

struct block_ptr
{
    std::size_t ptr;
    std::size_t size;
};

using block = std::variant<
    block_byte,
    block_int,
    block_uint,
    block_float,
    block_ptr
>;

struct object
{
    std::vector<block> data;
    anzu::type_name    type;
};

auto to_string(const block& blk) -> std::string;
auto to_string(const object& object) -> std::string;

auto make_int(std::int64_t val) -> object;
auto make_uint(std::uint64_t val) -> object;
auto make_char(char val) -> object;
auto make_float(double val) -> object;
auto make_bool(bool val) -> object;
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