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

auto list_repr(const block_list& list) -> std::string
{
    const auto to_repr = [](const auto& o) { return to_string(o); };
    return std::format("[{}]", anzu::format_comma_separated(*list, to_repr));
}

}

auto to_string(const block& blk) -> std::string
{
    return std::visit(overloaded {
        [](block_int val) { return std::to_string(val); },
        [](block_bool val) { return std::string{val ? "true" : "false"}; },
        [](const block_str& v) { return std::format("'{}'", v); },
        [](const block_list& v) { return list_repr(v); },
        [](block_null) { return std::string{"null"}; }
    }, blk.as_variant());
}

auto to_string(const object_def& object) -> std::string
{
    return std::format(
        "{}({})",
        to_string(object.type),
        format_comma_separated(object.data, [](const auto& b) { return to_string(b); })
    );
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

auto is_int(std::string_view token) -> bool
{
    auto it = token.begin();
    if (token.starts_with("-")) {
        std::advance(it, 1);
    }
    return std::all_of(it, token.end(), [](char c) { return std::isdigit(c); });
}

auto to_int(std::string_view token) -> int
{
    if (!is_int(token)) {
        anzu::print("type error: cannot convert '{}' to int\n", token);
        std::exit(1);
    }
    return std::stoi(std::string{token});
}

}