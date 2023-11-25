#include "object.hpp"
#include "token.hpp"
#include "utility/common.hpp"

#include <cassert>
#include <algorithm>
#include <ranges>
#include <string_view>

namespace anzu {
namespace {

static constexpr auto PTR_SIZE = std::size_t{8};

}

auto type_name::is_ptr() const -> bool
{
    return std::holds_alternative<type_ptr>(*this);
}

auto type_name::add_ptr() const -> type_name
{
    if (is_ptr()) return *this;
    return { type_ptr{ .inner_type{*this} } };
}

auto type_name::remove_ptr() const -> type_name
{
    if (!is_ptr()) return *this;
    return *std::get<type_ptr>(*this).inner_type;
}

auto type_name::is_const() const -> bool
{
    return std::holds_alternative<type_const>(*this);
}

auto type_name::add_const() const -> type_name
{
    if (is_const()) return *this;
    return { type_const{ .inner_type{*this} } };
}

auto type_name::remove_const() const -> type_name
{
    if (!is_const()) return *this;
    return *std::get<type_const>(*this).inner_type;
}

auto type_name::strip_const() const -> std::pair<type_name, bool>
{
    return {remove_const(), is_const()};
}

auto to_string_paren(const type_name& type) -> std::string
{
    const auto str = to_string(type);
    if (str.contains(' ')) {
        return std::format("({})", str);
    }
    return str;
}

auto to_string(const type_name& type) -> std::string
{
    return std::visit([](const auto& t) { return ::anzu::to_string(t); }, type);
}

auto to_string(type_fundamental t) -> std::string
{
    switch (t.type) {
        case fundamental::null_type: return "null";
        case fundamental::bool_type: return "bool";
        case fundamental::char_type: return "char";
        case fundamental::i32_type:  return "i32";
        case fundamental::i64_type:  return "i64";
        case fundamental::u64_type:  return "u64";
        case fundamental::f64_type:  return "f64";
        default: return "UNKNOWN";
    }
}

auto to_string(const type_struct& type) -> std::string
{
    return type.name;
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
        format_comma_separated(type.param_types, to_string_paren),
        *type.return_type
    );
}

auto to_string(const type_const& type) -> std::string
{
    return std::format("const {}", to_string(*type.inner_type));
}

auto hash(const type_name& type) -> std::size_t
{
    return std::visit([](const auto& t) { return hash(t); }, type);
}

auto hash(type_fundamental type) -> std::size_t
{
    return static_cast<std::size_t>(type.type);
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
    static const auto base = std::hash<std::string_view>{}("type_ptr");
    return hash(*type.inner_type) ^ base;
}

auto hash(const type_span& type) -> std::size_t
{
    static const auto base = std::hash<std::string_view>{}("type_span");
    return hash(*type.inner_type) ^ base;
}

auto hash(const type_function_ptr& type) -> std::size_t
{
    auto val = hash(*type.return_type);
    for (const auto& param : type.param_types) {
        val ^= hash(param);
    }
    return val;
}

auto hash(const type_const& type) -> std::size_t
{
    static const auto base = std::hash<std::string_view>{}("type_const");
    return hash(*type.inner_type) ^ base;
}

auto null_type() -> type_name
{
    return {type_fundamental { .type = fundamental::null_type }};
}

auto bool_type() -> type_name
{
    return {type_fundamental { .type = fundamental::bool_type }};
}

auto char_type() -> type_name
{
    return {type_fundamental { .type = fundamental::char_type }};
}

auto i32_type() -> type_name
{
    return {type_fundamental { .type = fundamental::i32_type }};
}

auto i64_type() -> type_name
{
    return {type_fundamental { .type = fundamental::i64_type }};
}

auto u64_type() -> type_name
{
    return {type_fundamental { .type = fundamental::u64_type }};
}

auto f64_type() -> type_name
{
    return {type_fundamental { .type = fundamental::f64_type }};
}

auto make_type(const std::string& name) -> type_name
{
    return { type_struct{ .name=name } };
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

auto inner_type(const type_name& t) -> type_name
{
    if (t.is_array()) {
        return *std::get<type_array>(t).inner_type;
    }
    if (t.is_ptr()) {
        return *std::get<type_ptr>(t).inner_type;
    }
    if (t.is_span()) {
        return *std::get<type_span>(t).inner_type; 
    }
    if (t.is_const()) {
        return t.remove_const();
    }
    panic("tried to get the inner type of an invalid type category, "
          "can only get the inner type for arrays, pointers, spans and references");
}

auto array_length(const type_name& t) -> std::size_t
{
    const auto mut_type = t.remove_const();
    panic_if(!mut_type.is_array(), "Tried to get length of a non-array type");
    return std::get<type_array>(mut_type).count;
}

auto size_of_ptr() -> std::size_t
{
    return PTR_SIZE;
}

auto is_type_trivially_copyable(const type_name& type) -> bool
{
    // TODO: Allow for trivially copyable struct types
    return std::visit(overloaded{
        [](type_fundamental)         { return true; },
        [](const type_struct&)       { return false; },
        [](const type_array& t)      { return is_type_trivially_copyable(*t.inner_type); },
        [](const type_span&)         { return true; },
        [](const type_ptr&)          { return true; },
        [](const type_function_ptr&) { return true; },
        [](const type_const& t)      { return is_type_trivially_copyable(*t.inner_type); }
    }, type);
}

auto type_store::add(const type_name& name, const type_fields& fields) -> bool
{
    if (d_classes.contains(name)) {
        return false;
    }
    d_classes.emplace(name, fields);
    return true;
}

auto type_store::contains(const type_name& type) const -> bool
{
    return std::visit(overloaded{
        [](type_fundamental)          { return true; },
        [&](const type_struct&)       { return d_classes.contains(type); },
        [&](const type_array& t)      { return contains(*t.inner_type); },
        [&](const type_span& t)       { return contains(*t.inner_type); },
        [&](const type_ptr& t)        { return contains(*t.inner_type); },
        [&](const type_function_ptr&) { return true; },
        [&](const type_const& t)      { return contains(*t.inner_type); }
    }, type);
}

// TODO: Refactor this mess
auto type_store::size_of(const type_name& type) const -> std::size_t
{
    return std::visit(overloaded{
        [](type_fundamental t) -> std::size_t {
            switch (t.type) {
                case fundamental::null_type:
                case fundamental::bool_type:
                case fundamental::char_type:
                    return 1;
                case fundamental::i32_type:
                    return 4;
                case fundamental::i64_type:
                case fundamental::u64_type:
                case fundamental::f64_type:
                    return 8;
                default:
                    panic("unknown fundamental type");
                return 0;
            }
        },
        [&](const type_struct& t) -> std::size_t {
            if (!d_classes.contains(type)) {
                panic("unknown type '{}'", type);
            }
            auto size = std::size_t{0};
            for (const auto& field : fields_of(type)) {
                size += size_of(field.type);
            }
            return std::max(std::size_t{1}, size); // empty structs take up one byte
        },
        [&](const type_array& t) {
            return size_of(*t.inner_type) * t.count;
        },
        [](const type_ptr&) {
            return PTR_SIZE;
        },
        [](const type_span&) {
            return 2 * PTR_SIZE; // actually a pointer + a size, but they are both 8 bytes
        },
        [](const type_function_ptr&) {
            return PTR_SIZE;
        },
        [&](const type_const& t) {
            return size_of(*t.inner_type);
        }
    }, type);
}

auto type_store::fields_of(const type_name& t) const -> type_fields
{
    if (auto it = d_classes.find(t.remove_const()); it != d_classes.end()) {
        return it->second.fields;
    }
    return {};
}

}
