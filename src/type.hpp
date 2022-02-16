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
    auto operator==(const type_simple&) const -> bool = default;
};

struct type_compound
{
    std::string       name;
    std::vector<type> subtypes;
    auto operator==(const type_compound&) const -> bool = default;
};

struct type_generic
{
    int id;
    auto operator==(const type_generic&) const -> bool = default;
};

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
auto make_generic(int id) -> type;

auto make_list_of(const type& t) -> type;
auto make_list_generic() -> type;

auto is_type_complete(const type& type) -> bool;

auto match(const type& concrete, const type& pattern) -> std::optional<std::unordered_map<int, type>>;
auto is_match(const type& concrete, const type& pattern) -> bool;

// Given an incomplete type and a map of types, replace the generics in the incomplete type
// with those from the map.
auto fill_type(const type& incomplete, const std::unordered_map<int, type>& matches) -> type;

struct signature
{
    struct arg
    {
        std::string name;
        anzu::type  type;
        auto operator==(const arg&) const -> bool = default;
    };

    std::vector<arg> args;
    anzu::type       return_type;
    auto operator==(const signature&) const -> bool = default;
};

auto to_string(const signature& sig) -> std::string;

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
};

}

template <>
struct std::formatter<anzu::type> : std::formatter<std::string>
{
    auto format(const anzu::type& type, auto& ctx) {
        return std::formatter<std::string>::format(anzu::to_string(type), ctx);
    }
};

template <>
struct std::formatter<anzu::signature> : std::formatter<std::string>
{
    auto format(const anzu::signature& type, auto& ctx) {
        return std::formatter<std::string>::format(anzu::to_string(type), ctx);
    }
};