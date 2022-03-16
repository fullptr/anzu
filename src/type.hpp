#pragma once
#include "vocabulary.hpp"

#include <string>
#include <variant>
#include <format>
#include <vector>
#include <optional>
#include <unordered_set>
#include <unordered_map>
#include <utility>

namespace anzu {

struct type_name;

struct type_simple
{
    std::string name;
    auto operator==(const type_simple&) const -> bool = default;
};

struct type_compound
{
    std::string       name;
    std::vector<type_name> subtypes;
    auto operator==(const type_compound&) const -> bool = default;
};

struct type_generic
{
    int id;
    auto operator==(const type_generic&) const -> bool = default;
};

struct type_name : public std::variant<type_simple, type_compound, type_generic> {};


struct field
{
    std::string name;
    type_name   type;
    auto operator==(const field&) const -> bool = default;
};

using type_fields = std::vector<field>;

auto to_string(const type_name& type) -> std::string;
auto to_string(const type_simple& type) -> std::string;
auto to_string(const type_compound& type) -> std::string;
auto to_string(const type_generic& type) -> std::string;

auto hash(const type_name& type) -> std::size_t;
auto hash(const type_simple& type) -> std::size_t;
auto hash(const type_compound& type) -> std::size_t;
auto hash(const type_generic& type) -> std::size_t;

auto int_type() -> type_name;
auto bool_type() -> type_name;
auto str_type() -> type_name;
auto null_type() -> type_name;
auto generic_type(int id) -> type_name;
auto vec2_type() -> type_name;

auto concrete_list_type(const type_name& t) -> type_name;
auto generic_list_type() -> type_name;

auto is_type_complete(const type_name& t) -> bool;

// Returns true if and only if the type is not a class type.
auto it_type_fundamental(const type_name& t) -> bool;

using match_result = std::unordered_map<int, type_name>;
auto match(const type_name& concrete, const type_name& pattern) -> std::optional<match_result>;

// Given an incomplete type and a map of types, replace the generics in the incomplete type
// with those from the map.
auto bind_generics(const type_name& incomplete, const match_result& matches) -> type_name;

struct signature
{
    struct arg
    {
        std::string name;
        anzu::type_name  type;
        auto operator==(const arg&) const -> bool = default;
    };

    std::vector<arg> args;
    anzu::type_name       return_type;
    auto operator==(const signature&) const -> bool = default;
};

auto to_string(const signature& sig) -> std::string;

class type_store
{
    using type_hash = decltype([](const type_name& t) { return anzu::hash(t); });
    std::unordered_set<type_name, type_hash> d_types;
    std::unordered_set<type_name, type_hash> d_generics;

    std::unordered_map<type_name, type_fields, type_hash> d_classes;

public:
    type_store();

    // Checks if the given type is registered or matches a registered generic.
    auto is_registered_type(const type_name& t) const -> bool;

    // Finds the given type with the given name if it exists, otherwise returns
    // nullptr. This is currently O(n) so we should potentially optimise this in
    // the future.
    auto find_by_name(const std::string& name) const -> const type_name*;

    // Given a type name, return the size of the type in blocks.
    auto block_size(const type_name& t) const -> std::size_t;
};

}

template <>
struct std::formatter<anzu::type_name> : std::formatter<std::string>
{
    auto format(const anzu::type_name& type, auto& ctx) {
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

template <>
struct std::hash<anzu::signature>
{
    auto operator()(const anzu::signature& sig) const -> std::size_t
    {
        auto ret = anzu::hash(sig.return_type);
        for (const auto& arg : sig.args) {
            ret ^= anzu::hash(arg.type);
        }
        return ret;
    }
};