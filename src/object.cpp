#include "object.hpp"
#include "token.hpp"
#include "utility/common.hpp"

#include <cassert>
#include <algorithm>
#include <ranges>
#include <string_view>

namespace anzu {

auto type_name::is_fundamental() const -> bool
{
    return std::holds_alternative<type_fundamental>(*this);
}

auto type_name::is_ptr() const -> bool
{
    return std::holds_alternative<type_ptr>(*this);
}

auto type_name::add_ptr() const -> type_name
{
    return { type_ptr{ .inner_type{*this} } };
}

auto type_name::remove_ptr() const -> type_name
{
    if (!is_ptr()) panic("tried to remove_ptr on a non-ptr type\n");
    return *std::get<type_ptr>(*this).inner_type;
}

auto type_name::add_const() const -> type_name
{
    auto copy = *this;
    copy.is_const = true;
    return copy;
}

auto type_name::remove_const() const -> type_name
{
    auto copy = *this;
    copy.is_const = false;
    return copy;
}

auto to_string_paren(const type_name& type) -> std::string
{
    const auto str = to_string(type);
    if (type.is_function_ptr()) {
        return std::format("({})", str);
    }
    return str;
}

auto to_string(const type_name& type) -> std::string
{
    const auto string_inner = std::visit([](const auto& t) {
        return ::anzu::to_string(t);
    }, type);
    return type.is_const ? std::format("{} const", string_inner) : string_inner;
}

auto to_string(type_fundamental t) -> std::string
{
    switch (t) {
        case type_fundamental::null_type:    return "null";
        case type_fundamental::bool_type:    return "bool";
        case type_fundamental::char_type:    return "char";
        case type_fundamental::i32_type:     return "i32";
        case type_fundamental::i64_type:     return "i64";
        case type_fundamental::u64_type:     return "u64";
        case type_fundamental::f64_type:     return "f64";
        case type_fundamental::nullptr_type: return "nullptr";
        default: return "UNKNOWN FUNDAMENTAL";
    }
}

auto to_string(const type_struct& type) -> std::string
{
    return std::format("<{}>.{}", type.module.string(), type.name);
}

auto to_string(const type_array& type) -> std::string
{
    return std::format("{}[{}]", to_string_paren(*type.inner_type), type.count);
}

auto to_string(const type_ptr& type) -> std::string
{
    return std::format("{}&", to_string_paren(*type.inner_type));
}

auto to_string(const type_span& type) -> std::string
{
    return std::format("{}[]", to_string_paren(*type.inner_type));
}

auto to_string(const type_function_ptr& type) -> std::string
{
    return std::format(
        "{}({}) -> {}",
        to_string(token_type::kw_function),
        format_comma_separated(type.param_types),
        to_string_paren(*type.return_type)
    );
}

auto to_string(const type_builtin& type) -> std::string
{
    return std::format(
        "<builtin: '{} {}({}) -> {}'>",
        to_string(token_type::kw_function),
        type.name,
        format_comma_separated(type.args),
        to_string_paren(*type.return_type)
    );
}

auto to_string(const type_bound_method& type) -> std::string
{
    return std::format(
        "<bound method: '{} {}({}) -> {}'>",
        to_string(token_type::kw_function),
        type.function_name,
        format_comma_separated(type.param_types),
        to_string_paren(*type.return_type)
    );
}

auto to_string(const type_bound_builtin_method& type) -> std::string
{
    return std::format("<bound builtin method: {}.{}>", type.name, *type.type);
}

auto to_string(const type_arena& type) -> std::string
{
    return std::string{"<arena>"};
}

auto to_string(const type_type& type) -> std::string
{
    return std::format("<type-expression: {}>", *type.type_val);
}

auto to_string(const type_module& type) -> std::string
{
    return std::format("<module: {}>", type.filepath.string());
}


auto hash(const type_name& type) -> std::size_t
{
    return std::visit([](const auto& t) { return hash(t); }, type);
}

auto hash(type_fundamental type) -> std::size_t
{
    return static_cast<std::size_t>(type);
}

auto hash(const type_struct& type) -> std::size_t
{
    return std::hash<std::string>{}(type.name);
}

auto hash(const type_array& type) -> std::size_t
{
    return hash(*type.inner_type) ^ std::hash<std::size_t>{}(type.count);
}

auto hash(const type_ptr& type) -> std::size_t
{
    return hash(*type.inner_type) ^ std::hash<std::string_view>{}("type_ptr");
}

auto hash(const type_span& type) -> std::size_t
{
    return hash(*type.inner_type) ^ std::hash<std::string_view>{}("type_span");
}

auto hash(const type_function_ptr& type) -> std::size_t
{
    auto val = hash(*type.return_type);
    for (const auto& param : type.param_types) {
        val ^= hash(param);
    }
    return val;
}

auto hash(const type_builtin& type) -> std::size_t
{
    auto val = hash(*type.return_type) ^ std::hash<std::string>{}(type.name);
    for (const auto& param : type.args) {
        val ^= hash(param);
    }
    return val;
}

auto hash(const type_bound_method& type) -> std::size_t
{
    auto val = hash(*type.return_type) ^ std::hash<std::size_t>{}(type.function_id);
    for (const auto& param : type.param_types) {
        val ^= hash(param);
    }
    return val;
}

auto hash(const type_bound_builtin_method& type) -> std::size_t
{
    return hash(*type.type) ^ std::hash<std::string>{}(type.name);
}

auto hash(const type_arena& type) -> std::size_t
{
    return std::hash<std::string_view>{}("type_arena");
}

auto hash(const type_type& type) -> std::size_t
{
    return hash(*type.type_val) ^ std::hash<std::string_view>{}("type_type");
}

auto hash(const type_module& type) -> std::size_t
{
    return std::hash<std::string>{}(type.filepath.string()) ^ std::hash<std::string_view>{}("type_module");
}

auto hash(std::span<const type_name> types) -> std::size_t
{
    auto hash_value = size_t{0};
    for (const auto& type : types) {
        hash_value ^= hash(type);
    }
    return hash_value;
}

auto null_type() -> type_name
{
    return {type_fundamental::null_type};
}

auto nullptr_type() -> type_name
{
    return {type_fundamental::nullptr_type};
}

auto bool_type() -> type_name
{
    return {type_fundamental::bool_type};
}

auto char_type() -> type_name
{
    return {type_fundamental::char_type};
}

auto i32_type() -> type_name
{
    return {type_fundamental::i32_type};
}

auto i64_type() -> type_name
{
    return {type_fundamental::i64_type};
}

auto u64_type() -> type_name
{
    return {type_fundamental::u64_type};
}

auto f64_type() -> type_name
{
    return {type_fundamental::f64_type};
}

auto arena_type() -> type_name
{
    return {type_arena{}};
}

auto string_literal_type() -> type_name
{
    return char_type().add_const().add_span();
}

auto type_name::is_array() const -> bool
{
    return std::holds_alternative<type_array>(*this);
}

auto type_name::add_array(std::size_t size) const -> type_name
{
    return {type_array{ .inner_type = { *this }, .count = size }};
}

auto type_name::remove_array() const -> type_name
{
    panic_if(!is_array(), "Tried to strip array from non-array type {}", *this);
    return *std::get<type_array>(*this).inner_type;
}

auto type_name::is_span() const -> bool
{
    return std::holds_alternative<type_span>(*this);
}

auto type_name::add_span() const -> type_name
{
    return {type_span{ .inner_type = { *this } }};
}

auto type_name::remove_span() const -> type_name
{
    panic_if(!is_span(), "Tried to strip span from non-span type {}", *this);
    return *std::get<type_span>(*this).inner_type;
}

auto type_name::is_function_ptr() const -> bool
{
    return std::holds_alternative<type_function_ptr>(*this);
}

auto type_name::is_builtin() const -> bool
{
    return std::holds_alternative<type_builtin>(*this);
}


auto type_name::is_bound_method() const -> bool
{
    return std::holds_alternative<type_bound_method>(*this);
}

auto type_name::is_bound_builtin_method() const -> bool
{
    return std::holds_alternative<type_bound_builtin_method>(*this);
}


auto type_name::is_arena() const -> bool
{
    return std::holds_alternative<type_arena>(*this);
}

auto type_name::is_type_value() const -> bool
{
    return std::holds_alternative<type_type>(*this);
}

auto type_name::is_module_value() const -> bool
{
    return std::holds_alternative<type_module>(*this);
}

auto inner_type(const type_name& t) -> type_name
{
    if (t.is_array()) {
        return *std::get<type_array>(t).inner_type;
    }
    if (t.is_span()) {
        return *std::get<type_span>(t).inner_type; 
    }
    if (t.is_type_value()) {
        return *std::get<type_type>(t).type_val; 
    }
    panic("tried to get the inner type of an invalid type category, "
          "can only get the inner type for arrays, spans and type values, type={}", t);
}

auto array_length(const type_name& t) -> std::size_t
{
    const auto mut_type = t.remove_const();
    panic_if(!mut_type.is_array(), "Tried to get length of a non-array type");
    return std::get<type_array>(mut_type).count;
}

}
