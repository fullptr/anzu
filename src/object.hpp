#pragma once
#include <string>
#include <variant>
#include <fmt/format.h>

namespace anzu {

using object = std::variant<int, bool, std::string>;

auto is_int(const std::string& token) -> bool;
auto to_int(const std::string& token) -> int;

}

template <> struct fmt::formatter<anzu::object> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.end(); }
    auto format(const anzu::object& obj, auto& ctx) {
        return std::visit([&](const auto& o) {
            return format_to(ctx.out(), "{}", o);
        }, obj);
    }
};