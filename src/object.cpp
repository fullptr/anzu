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

auto to_string(const block& blk) -> std::string
{
    return std::visit(overloaded {
        [](block_byte byte) { return std::format("{:x}", static_cast<unsigned char>(byte)); },
        [](block_ptr ptr) { return std::format("[{:x}:{}]", ptr.ptr, ptr.size); },
        [](block_uint val) { return std::format("{}u", val); },
        [](auto&& val) { return std::format("{}", val); }
    }, blk);
}

auto to_string(const object& object) -> std::string
{
    return std::format("{}({})", object.type, format_comma_separated(object.data));
}

auto make_int(block_int val) -> object
{
    return { .data = { block_int{val} }, .type = int_type() };
}

auto make_uint(block_uint val) -> object
{
    return { .data = { val }, .type = uint_type() };
}

auto make_char(block_byte val) -> object
{
    return { .data = { val }, .type = char_type() };
}

auto make_float(block_float val) -> object
{
    return { .data = { block_float{val} }, .type = float_type() };
}

auto make_bool(bool val) -> object
{
    const auto v = val ? block_byte{1} : block_byte{0};
    return { .data = { v }, .type = bool_type() };
}

auto make_null() -> object
{
    return { .data = { block_byte{0} }, .type = null_type() };
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