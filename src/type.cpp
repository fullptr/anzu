#include "type.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/views.hpp"

#include <algorithm>
#include <cassert>

namespace anzu {

auto to_string(const type_name& type) -> std::string
{
    return std::visit([](const auto& t) { return to_string(t); }, type);
}

auto to_string(const type_simple& type) -> std::string
{
    return type.name;
}

auto to_string(const type_list& type) -> std::string
{
    return std::format("{}[{}]", to_string(type.inner_type[0]), type.count);
}

auto to_string(const type_ptr& type) -> std::string
{
    return std::format("&{}", to_string(type.inner_type[0]));
}

auto hash(const type_name& type) -> std::size_t
{
    return std::visit([](const auto& t) { return hash(t); }, type);
}

auto hash(const type_simple& type) -> std::size_t
{
    return std::hash<std::string>{}(type.name);
}

auto hash(const type_list& type) -> std::size_t
{
    return hash(type.inner_type[0]) ^ std::hash<std::size_t>{}(type.count);
}

auto hash(const type_ptr& type) -> std::size_t
{
    return hash(type.inner_type[0]) ^ std::hash<std::size_t>{}(100);
}

auto int_type()  -> type_name
{
    return {type_simple{ .name = std::string{tk_int} }};
}

auto float_type() -> type_name
{
    return {type_simple{ .name = std::string{tk_float} }};
}

auto bool_type() -> type_name
{
    return {type_simple{ .name = std::string{tk_bool} }};
}

auto str_type()  -> type_name
{
    return {type_simple{ .name = std::string{tk_str} }};
}

auto null_type() -> type_name
{
    return {type_simple{ .name = std::string{tk_null} }};
}

auto concrete_list_type(const type_name& t, std::size_t size) -> type_name
{
    return {type_list{ .inner_type = { t }, .count = size }};
}

auto is_list_type(const type_name& t) -> bool
{
    return std::holds_alternative<type_list>(t);
}

auto concrete_ptr_type(const type_name& t) -> type_name
{
    return {type_ptr{ .inner_type = { t } }};
}

auto is_ptr_type(const type_name& t) -> bool
{
    return std::holds_alternative<type_ptr>(t);
}

auto inner_type(const type_name& t) -> type_name
{
    if (is_list_type(t)) {
        return std::get<type_list>(t).inner_type[0];
    }
    if (is_ptr_type(t)) {
        return std::get<type_ptr>(t).inner_type[0];
    }
    print("OH NO MY TYPE\n");
    std::exit(1);
    return {};
}

auto is_type_fundamental(const type_name& type) -> bool
{
    return type == int_type()
        || type == float_type()
        || type == bool_type()
        || type == str_type()
        || type == null_type()
        || is_list_type(type)
        || is_ptr_type(type);
}

// Loads each key/value pair from src into dst. If the key already exists in dst and has a
// different value, stop and return false.
auto update(
    std::unordered_map<int, type_name>& dst, const std::unordered_map<int, type_name>& src
)
    -> bool
{
    for (const auto& [key, value] : src) {
        if (auto it = dst.find(key); it != dst.end()) {
            if (it->second != value) {
                return false;
            }
        } else {
            dst.emplace(key, value);
        }
    }
    return true;
}

auto to_string(const signature& sig) -> std::string
{
    const auto proj = [](const auto& arg) { return arg.type; };
    return std::format("({}) -> {}", format_comma_separated(sig.args, proj), sig.return_type);
}

type_store::type_store()
{
}

auto type_store::is_valid(const type_name& t) const -> bool
{
    return is_type_fundamental(t) || d_classes.contains(t);
}

auto type_store::size_of(const type_name& t) const -> std::size_t
{
    if (std::holds_alternative<type_list>(t)) {
        const auto& list = std::get<type_list>(t);
        return size_of(list.inner_type[0]) * list.count;
    }
    if (std::holds_alternative<type_ptr>(t)) {
        return 1;
    }

    if (auto it = d_classes.find(t); it != d_classes.end()) {
        auto size = std::size_t{0};
        for (const auto& field : it->second) {
            size += size_of(field.type);
        }
        return size;
    }

    return 1; // By default, assume block size of 1 (should we have this?)
}

auto type_store::fields_of(const type_name& t) const -> type_fields
{
    if (auto it = d_classes.find(t); it != d_classes.end()) {
        return it->second;
    }
    if (is_type_fundamental(t)) {
        return {{ .name = "blk", .type = t }};
    }
    return {};
}

auto type_store::register_type(const type_name& name, const type_fields& fields) -> bool
{
    if (d_classes.contains(name)) {
        return false;
    }
    d_classes.emplace(name, fields);
    return true;
}

}