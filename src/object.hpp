#pragma once
#include <format>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "type.hpp"
#include "utility/print.hpp"

namespace anzu {

class block;

struct object_def
{
    std::vector<block> data;
    anzu::type         type;
};

using block_int  = int;
using block_bool = bool;
using block_str  = std::string;
using block_list = std::shared_ptr<std::vector<block>>;
using block_null = std::monostate;

class block
{
public:
    using block_type = std::variant<
        block_int,
        block_bool,
        block_str,
        block_list,
        block_null
    >;

private:
    block_type d_value;

public:
    template <typename Obj>
    explicit block(const Obj& obj) : d_value{obj} {}
    
    block() : d_value{0} {}

    // Casts, for certain types, converts the object to the requested type.
    auto to_int() const -> int;
    auto to_bool() const -> bool;
    auto to_str() const -> std::string;

    auto as_variant() const -> const block_type& { return d_value; }

    template <typename T>
    auto is() const -> bool
    {
        return std::holds_alternative<T>(d_value);
    }
    
    template <typename T>
    auto as() -> T&
    {
        if (!is<T>()) {
            anzu::print("error: {} does not contain requested type\n", to_repr());
            std::exit(1);
        }
        return std::get<T>(d_value);
    }

    template <typename T>
    auto as() const -> const T&
    {
        if (!is<T>()) {
            anzu::print("error: {} does not contain requested type\n", to_repr());
            std::exit(1);
        }
        return std::get<T>(d_value);
    }

    auto to_repr() const -> std::string;

    friend auto operator+(const block& lhs, const block& rhs) -> block;
    friend auto operator-(const block& lhs, const block& rhs) -> block;
    friend auto operator*(const block& lhs, const block& rhs) -> block;
    friend auto operator/(const block& lhs, const block& rhs) -> block;
    friend auto operator%(const block& lhs, const block& rhs) -> block;
    
    friend auto operator||(const block& lhs, const block& rhs) -> bool;
    friend auto operator&&(const block& lhs, const block& rhs) -> bool;

    friend auto operator<=>(const block& lhs, const block& rhs) -> std::strong_ordering = default;

    friend auto swap(block& lhs, block& rhs) -> void;
};

auto to_string(const object_def& object) -> std::string;

inline auto null_object() -> object_def
{
    return { .data = { block{block_null()} }, .type = null_type() };
}

auto is_int(std::string_view token) -> bool;
auto to_int(std::string_view token) -> int;

auto format_special_chars(const std::string& str) -> std::string;

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
        const auto str = fmt == 's' ? blk.to_str() : blk.to_repr();
        return std::formatter<std::string>::format(std::format("{}", str), ctx);
    }
};