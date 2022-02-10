#pragma once
#include "vocabulary.hpp"

#include <string>
#include <variant>
#include <vector>
#include <unordered_map>

namespace anzu {

struct type;

struct type_simple
{
    std::string name;
};

struct type_compound
{
    std::string       name;
    std::vector<type> subtypes;
};

struct type_generic
{
    int id;
};

struct type : std::variant<type_simple, type_compound, type_generic> {};

auto to_string(const type& type) -> std::string;
auto to_string(const type_simple& type) -> std::string;
auto to_string(const type_compound& type) -> std::string;
auto to_string(const type_generic& type) -> std::string;

auto hash(const type& type) -> std::size_t;
auto hash(const type_simple& type) -> std::size_t;
auto hash(const type_compound& type) -> std::size_t;
auto hash(const type_generic& type) -> std::size_t;

inline auto make_int()  -> type { return {type_simple{ .name = std::string{tk_int}       }}; }
inline auto make_bool() -> type { return {type_simple{ .name = std::string{tk_bool}      }}; }
inline auto make_str()  -> type { return {type_simple{ .name = std::string{tk_str}       }}; }
inline auto make_list() -> type { return {type_simple{ .name = std::string{tk_list}      }}; }
inline auto make_null() -> type { return {type_simple{ .name = std::string{tk_null_type} }}; }
inline auto make_any()  -> type { return {type_simple{ .name = std::string{tk_any}       }}; }
inline auto make_generic(int id) -> type { return {type_generic{ .id = id }}; }

inline auto make_list_generic() -> type
{
    return {type_compound{
        .name = std::string{tk_list},
        .subtypes = { make_generic(0) }
    }};
}

inline auto make_list_typed(const type& subtype) -> type
{
    return { type_simple{ .name = std::string{tk_int} } };
}

class type_store
{
    // key = string representation of type
    // value = type object
    std::unordered_map<std::string, type> d_types;

public:

};

}

template <>
struct std::hash<anzu::type>
{
    auto operator()(const anzu::type& t) -> std::size_t
    {
        return anzu::hash(t);
    }
};