#pragma once
#include "vocabulary.hpp"

#include <string>
#include <variant>
#include <format>
#include <vector>
#include <optional>
#include <unordered_set>
#include <unordered_map>

namespace anzu {

struct type;

struct type_simple
{
    std::string name;

};
inline auto operator==(const type_simple& lhs, const type_simple& rhs) -> bool
{
    return lhs.name == rhs.name;
}

struct type_compound
{
    std::string       name;
    std::vector<type> subtypes;

};
inline auto operator==(const type_compound& lhs, const type_compound& rhs) -> bool {
    return std::tie(lhs.name, lhs.subtypes) == std::tie(rhs.name, rhs.subtypes);
}

struct type_generic
{
    int id;

};
inline auto operator==(const type_generic& lhs, const type_generic& rhs) -> bool
{
    return lhs.id == rhs.id;
}

struct type : public std::variant<type_simple, type_compound, type_generic> {};

auto to_string(const type& type) -> std::string;
auto to_string(const type_simple& type) -> std::string;
auto to_string(const type_compound& type) -> std::string;
auto to_string(const type_generic& type) -> std::string;

auto hash(const type& type) -> std::size_t;
auto hash(const type_simple& type) -> std::size_t;
auto hash(const type_compound& type) -> std::size_t;
auto hash(const type_generic& type) -> std::size_t;

auto make_int()  -> type;
auto make_bool() -> type;
auto make_str()  -> type;
auto make_null() -> type;
auto make_any()  -> type;
auto make_generic(int id) -> type;

auto make_list_of(const type& t) -> type;
auto make_list_generic() -> type;

auto is_type_complete(const type& type) -> bool;

auto match(const type& concrete, const type& pattern) -> std::optional<std::unordered_map<int, type>>;
auto is_match(const type& concrete, const type& pattern) -> bool;

struct type_hash
{
    std::size_t operator()(const type& t) const noexcept { return anzu::hash(t); }
};

class type_store
{
    std::unordered_set<type, type_hash> d_types;
    std::unordered_set<type, type_hash> d_generics;

public:
    type_store();

    // Checks if the given type is registered or matches a registered generic.
    auto is_registered_type(const type& t) const -> bool;

    // auto register(const type& t) -> void;
};

}

template <> struct std::formatter<anzu::type> : std::formatter<std::string>
{
    auto format(const anzu::type& type, auto& ctx) {
        return std::formatter<std::string>::format(anzu::to_string(type), ctx);
    }
};