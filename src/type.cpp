#include "type.hpp"
#include "utility/print.hpp"

#include <algorithm>
#include <cassert>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

}

auto to_string(const type& type) -> std::string
{
    return std::visit([](const auto& t) { return to_string(t); }, type);
}

auto to_string(const type_simple& type) -> std::string
{
    return type.name;
}

auto to_string(const type_compound& type) -> std::string
{
    const auto subtypes = format_comma_separated(
        type.subtypes,
        [](const auto& t) { return to_string(t); }
    );
    return std::format("{}<{}>", type.name, subtypes);
}

auto to_string(const type_generic& type) -> std::string
{
    return std::format("[{}]", type.id);
}

auto hash(const type& type) -> std::size_t
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

auto make_int()  -> type
{
    return {type_simple{ .name = std::string{tk_int}  }};
}

auto make_bool() -> type
{
    return {type_simple{ .name = std::string{tk_bool} }};
}

auto make_str()  -> type
{
    return {type_simple{ .name = std::string{tk_str}  }};
}

auto make_null() -> type
{
    return {type_simple{ .name = std::string{tk_null} }};
}

auto make_any()  -> type
{
    return {type_simple{ .name = std::string{tk_any}  }};
}

auto make_generic(int id) -> type
{
    return {type_generic{ .id = id }};
}

auto make_list_of(const type& t) -> type
{
    return {type_compound{
        .name = std::string{tk_list},
        .subtypes = { t }
    }};
}

auto make_list_generic() -> type
{
    return {type_compound{
        .name = std::string{tk_list},
        .subtypes = { make_generic(0) }
    }};
}

auto is_type_complete(const type& t) -> bool
{
    return std::visit(overloaded {
        [](const type_simple&) {
            return true;
        },
        [](const type_generic&) {
            return false;
        },
        [](const type_compound& t) {
            return std::all_of(begin(t.subtypes), end(t.subtypes), [](const auto& st) {
                return is_type_complete(st);
            });
        }
    }, t);
}

auto match(const type& concrete, const type& pattern) -> std::optional<std::unordered_map<int, type>>
{
    assert(is_type_complete(concrete));

    if (std::holds_alternative<type_generic>(pattern)) {
        auto match_result = std::unordered_map<int, type>{};
        match_result.emplace(std::get<type_generic>(pattern).id, concrete);
        return match_result;
    }
    return {};
}

auto is_match(const type& concrete, const type& pattern) -> bool
{
    return match(concrete, pattern).has_value();
}

type_store::type_store()
{
    d_types.emplace(tk_int,  make_int());
    d_types.emplace(tk_bool, make_bool());
    d_types.emplace(tk_str,  make_str());
    d_types.emplace("list<[0]>", make_list_generic());
    d_types.emplace(tk_null, make_null());
    d_types.emplace(tk_any,  make_any());
}

auto type_store::is_registered_type(const type& t) -> bool
{
    return d_types.contains(to_string(t));
}

}