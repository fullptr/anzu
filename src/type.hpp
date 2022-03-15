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

struct type;

struct type_simple
{
    struct field;

    std::string name;
    std::optional<std::vector<field>> fields; // nullopt == fundamental type
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

struct type_simple::field
{
    std::string name;
    type        type;
    auto operator==(const field&) const -> bool = default;
};

auto to_string(const type& type) -> std::string;
auto to_string(const type_simple& type) -> std::string;
auto to_string(const type_compound& type) -> std::string;
auto to_string(const type_generic& type) -> std::string;

auto hash(const type& type) -> std::size_t;
auto hash(const type_simple& type) -> std::size_t;
auto hash(const type_compound& type) -> std::size_t;
auto hash(const type_generic& type) -> std::size_t;

auto int_type() -> type;
auto bool_type() -> type;
auto str_type() -> type;
auto null_type() -> type;
auto generic_type(int id) -> type;
auto vec2_type() -> type;

auto concrete_list_type(const type& t) -> type;
auto generic_list_type() -> type;

auto is_type_complete(const type& t) -> bool;

// Returns the number of blocks that represent this type. Returns 0 if the type is not complete.
auto type_block_size(const type& t) -> std::size_t;

// Returns true if and only if the type is not a class type.
auto it_type_fundamental(const type& t) -> bool;

using match_result = std::unordered_map<int, type>;
auto match(const type& concrete, const type& pattern) -> std::optional<match_result>;

// Given an incomplete type and a map of types, replace the generics in the incomplete type
// with those from the map.
auto bind_generics(const type& incomplete, const match_result& matches) -> type;

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

class type_store
{
    using type_hash = decltype([](const type& t) { return anzu::hash(t); });
    std::unordered_set<type, type_hash> d_types;
    std::unordered_set<type, type_hash> d_generics;

public:
    type_store();

    // Checks if the given type is registered or matches a registered generic.
    auto is_registered_type(const type& t) const -> bool;

    // Finds the given type with the given name if it exists, otherwise returns
    // nullptr. This is currently O(n) so we should potentially optimise this in
    // the future.
    auto find_by_name(const std::string& name) const -> const type*;
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