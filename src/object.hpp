#pragma once
#include <string>
#include <variant>
#include <fmt/format.h>

namespace anzu {

using object = std::variant<int, bool>;

bool is_literal(const std::string& token);
anzu::object parse_literal(const std::string& token);

}

template <> struct fmt::formatter<anzu::object> {
    constexpr auto parse(format_parse_context& ctx) -> decltype(ctx.begin()) {
        return ctx.end();
    }

    template <typename FormatContext>
    auto format(const anzu::object& obj, FormatContext& ctx) -> decltype(ctx.out()) {
        return std::visit([&](const auto& o) {
            return format_to(ctx.out(), "{}", o);
        }, obj);
    }
};