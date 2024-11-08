#include "object.hpp"
#include "token.hpp"
#include "utility/common.hpp"

#include <cassert>
#include <algorithm>
#include <ranges>
#include <string_view>

namespace anzu {

auto type_function_template::to_string() const -> std::string
{
    return anzu::to_string(*this);
}

auto type_function::to_pointer() const -> type_name
{
    return type_function_ptr{ param_types, return_type };
}

auto type_name::add_ptr() const -> type_name
{
    return { type_ptr{ .inner_type{*this} } };
}

auto type_name::remove_ptr() const -> type_name
{
    if (!is<type_ptr>()) panic("tried to remove_ptr on a non-ptr type\n");
    return *as<type_ptr>().inner_type;
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
    if (type.is<type_function_ptr>()) {
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
        default: return "UNKNOWN FUNDAMENTAL";
    }
}

auto to_string(const type_struct& type) -> std::string
{
    if (!type.templates.empty()) {
        return std::format("<{}>.{}!({})", type.module.string(), type.name, format_comma_separated(type.templates));
    } else {
        return std::format("<{}>.{}", type.module.string(), type.name);
    }
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
        "<bound_method: '{}({}) -> {}'>",
        to_string(token_type::kw_function),
        type.id,
        format_comma_separated(type.param_types),
        to_string_paren(*type.return_type)
    );
}

auto to_string(const type_bound_method_template& type) -> std::string
{
    return std::format(
        "<bound_method_template: <{}>.{}.{}>",
        type.module.string(),
        to_string(type.struct_name),
        type.name
    );
}

auto to_string(const type_arena& type) -> std::string
{
    return std::string{"<arena>"};
}

auto to_string(const type_type& type) -> std::string
{
    return std::format("<type: {}>", *type.type_val);
}

auto to_string(const type_function& type) -> std::string
{
    const auto function_ptr_type = type_function_ptr{type.param_types, type.return_type};
    return std::format("<function: id {} {}>", type.id, to_string(function_ptr_type));
}

auto to_string(const type_function_template& type) -> std::string
{
    return std::format("<function_template: <{}>.{}.{}>", type.module.string(), type.struct_name.name, type.name);
}

auto to_string(const type_struct_template& type) -> std::string
{
    return std::format("<struct_template: <{}>.{}>", type.module.string(), type.name);
}

auto to_string(const type_module& type) -> std::string
{
    return std::format("<module: {}>", type.filepath.string());
}

auto to_string(const type_ct_bool& type) -> std::string
{
    return std::format("<comptime_bool: {}>", type.value);
}

auto to_string(const type_placeholder& type) -> std::string
{
    return std::format("<placeholder: {}>", type.name);
}

auto null_type() -> type_name
{
    return {type_fundamental::null_type};
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

auto type_name::add_array(std::size_t size) const -> type_name
{
    return {type_array{ .inner_type = { *this }, .count = size }};
}

auto type_name::remove_array() const -> type_name
{
    panic_if(!is<type_array>(), "Tried to strip array from non-array type {}", *this);
    return *as<type_array>().inner_type;
}

auto type_name::add_span() const -> type_name
{
    return {type_span{ .inner_type = { *this } }};
}

auto type_name::remove_span() const -> type_name
{
    panic_if(!is<type_span>(), "Tried to strip span from non-span type {}", *this);
    return *as<type_span>().inner_type;
}

auto inner_type(const type_name& t) -> type_name
{
    if (auto type = t.get_if<type_array>()) {
        return *type->inner_type;
    }
    if (auto type = t.get_if<type_span>()) {
        return *type->inner_type; 
    }
    if (auto type = t.get_if<type_type>()) {
        return *type->type_val; 
    }
    panic("tried to get the inner type of an invalid type category, "
          "can only get the inner type for arrays, spans and type values, type={}", t);
}

}
