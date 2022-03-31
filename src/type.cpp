#include "type.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

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

auto to_string(const type_compound& type) -> std::string
{
    const auto subtypes = format_comma_separated(type.subtypes);
    return std::format("{}<{}>", type.name, subtypes);
}

auto to_string(const type_generic& type) -> std::string
{
    return std::format("[{}]", type.id);
}

auto hash(const type_name& type) -> std::size_t
{
    return std::visit([](const auto& t) { return hash(t); }, type);
}

auto hash(const type_simple& type) -> std::size_t
{
    return std::hash<std::string>{}(type.name);
}

auto hash(const type_compound& type) -> std::size_t
{
    auto hash_value = std::hash<std::string>{}(type.name);
    for (const auto& subtype : type.subtypes) {
        hash_value ^= hash(subtype);
    }
    return hash_value;
}

auto hash(const type_generic& type) -> std::size_t
{
    return std::hash<int>{}(type.id);
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

auto generic_type(int id) -> type_name
{
    return {type_generic{ .id = id }};
}

auto concrete_list_type(const type_name& t) -> type_name
{
    return {type_compound{
        .name = std::string{tk_list}, .subtypes = { t }
    }};
}

auto generic_list_type() -> type_name
{
    return {type_compound{
        .name = std::string{tk_list}, .subtypes = { generic_type(0) }
    }};
}

auto concrete_ptr_type(const type_name& t) -> type_name
{
    return {type_compound{
        .name = std::string{tk_ptr}, .subtypes = { t }
    }};
}

auto generic_ptr_type() -> type_name
{
    return {type_compound{
        .name = std::string{tk_ptr}, .subtypes = { generic_type(0) }
    }};
}

auto is_type_complete(const type_name& t) -> bool
{
    return std::visit(overloaded {
        [](const type_simple&) { return true; },
        [](const type_generic&) { return false; },
        [](const type_compound& t) {
            return std::all_of(begin(t.subtypes), end(t.subtypes), [](const auto& st) {
                return is_type_complete(st);
            });
        }
    }, t);
}

auto is_type_fundamental(const type_name& type) -> bool
{
    return type == int_type()
        || type == float_type()
        || type == bool_type()
        || type == str_type()
        || type == null_type()
        || match(type, generic_list_type())
        || match(type, generic_ptr_type());
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

auto match(const type_name& concrete, const type_name& pattern) -> std::optional<match_result>
{
    // Pre-condition, concrete must be a complete type (non-generic and no generic subtypes)
    if (!is_type_complete(concrete)) {
        return std::nullopt;
    }

    // Check 1: Trivial case - pattern is generic, matches entire concrete type
    if (const auto inner = std::get_if<type_generic>(&pattern)) {
        return match_result{
            { inner->id, concrete }
        };
    }

    // At this point, neither 'concrete' nor 'pattern' are generic

    // Check 2: Trivial case - equality implies match
    if (concrete == pattern) {
        return match_result{};
    }

    // If either are simple, there there is no match because:
    //   - if both are simple, they are not equal (check 2)
    //   - simple cannot match compound and vice versa
    if (std::holds_alternative<type_simple>(pattern) || std::holds_alternative<type_simple>(concrete)) {
        return std::nullopt; // No match
    }

    // At this point, both 'concrete' and 'pattern' are compound
    const auto& p = std::get<type_compound>(pattern);
    const auto& c = std::get<type_compound>(concrete);
    if (p.name != c.name || p.subtypes.size() != c.subtypes.size()) {
        return std::nullopt;
    }

    auto matches = match_result{};

    // Loop through the subtypes and do pairwise matches. Any successful matches should be
    // lifted into our match map. If an index is already in our map with a different type,
    // the match fails and we return nullopt. (std::views::zip in C++23 would be nice here)
    auto cit = c.subtypes.begin();
    auto pit = p.subtypes.begin();
    for (; cit != c.subtypes.end(); ++cit, ++pit) {
        const auto submatch = match(*cit, *pit);
        if (!submatch.has_value()) {
            return std::nullopt;
        }
        if (!update(matches, submatch.value())) {
            return std::nullopt;
        }
    }

    return matches;
}

auto replace(type_name& ret, const match_result& matches) -> void
{
    std::visit(overloaded {
        [&](type_simple&) {},
        [&](type_generic& type) {
            if (auto it = matches.find(type.id); it != matches.end()) {
                ret = it->second;
            }
        },
        [&](type_compound& type) {
            for (auto& subtype : type.subtypes) {
                replace(subtype, matches);
            }
        }
    }, ret);
}

auto bind_generics(const type_name& incomplete, const std::unordered_map<int, type_name>& matches) -> type_name
{
    auto ret_type = incomplete;
    replace(ret_type, matches);
    return ret_type;
}

auto to_string(const signature& sig) -> std::string
{
    const auto proj = [](const auto& arg) { return arg.type; };
    return std::format("({}) -> {}", format_comma_separated(sig.args, proj), sig.return_type);
}

type_store::type_store()
{
}

auto type_store::is_registered_type(const type_name& t) const -> bool
{
    if (is_type_fundamental(t)) {
        return true;
    }
    return d_classes.contains(t);
}

auto type_store::block_size(const type_name& t) const -> std::size_t
{
    if (!is_type_complete(t)) {
        return 1; // Hack to make lists work (for fundamentals) for now. Should be 0 or exception
    }

    if (auto it = d_classes.find(t); it != d_classes.end()) {
        auto size = std::size_t{0};
        for (const auto& field : it->second) {
            size += block_size(field.type);
        }
        return size;
    }

    return 1; // By default, assume block size of 1 (should we have this?)
}

auto type_store::get_fields(const type_name& t) const -> type_fields
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