#include "object.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <algorithm>
#include <ranges>
#include <string_view>

namespace anzu {
namespace {

auto format_error(const std::string& str) -> void
{
    anzu::print("format error: could not format special chars in '{}'\n", str);
    std::exit(1);
}

}

auto to_string(const object& object) -> std::string
{
    return std::format("{}({})", object.type, format_comma_separated(object.data));
}

auto make_i32(std::int32_t val) -> object
{
    return { .data = to_bytes(val), .type = i32_type() };
}

auto make_i64(std::int64_t val) -> object
{
    return { .data = to_bytes(val), .type = i64_type() };
}

auto make_u64(std::uint64_t val) -> object
{
    return { .data = to_bytes(val), .type = u64_type() };
}

auto make_f64(double val) -> object
{
    return { .data = to_bytes(val), .type = f64_type() };
}

auto make_char(char val) -> object
{
    return { .data = { static_cast<std::byte>(val) }, .type = char_type() };
}

auto make_bool(bool val) -> object
{
    const auto v = val ? std::byte{1} : std::byte{0};
    return { .data = { v }, .type = bool_type() };
}

auto make_null() -> object
{
    return { .data = { std::byte{0} }, .type = null_type() };
}

auto format_special_chars(const std::string& str) -> std::string
{
    std::string ret;
    for (auto it = str.begin(); it != str.end(); ++it) {
        if (*it == '\\') {
            if (++it == str.end()) { format_error(str); }
            switch (*it) {
                break; case 'n': ret += '\n';
                break; case 't': ret += '\t';
                break; case 'r': ret += '\r';
                break; default: format_error(str);
            }
        }
        else {
            ret += *it;
        }
    }
    return ret;
}

}