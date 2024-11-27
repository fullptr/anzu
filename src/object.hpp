#pragma once
#include <format>
#include <memory>
#include <string>
#include <variant>
#include <unordered_map>
#include <vector>
#include <span>
#include <filesystem>
#include <utility>
#include <concepts>

#include "utility/common.hpp"
#include "utility/value_ptr.hpp"
#include "utility/hash.hpp"

namespace anzu {

// Want these to be equivalent since we want uints available in the runtime but we also want
// to use it as indexes into C++ vectors which use size_t.
static_assert(std::is_same_v<std::uint64_t, std::size_t>);

struct type_name;
struct null_tag{};

using const_value = std::variant<
    std::monostate,       // no value
    null_tag,             // null
    bool,                 // bool
    char,                 // char
    std::int32_t,         // i32
    std::int64_t,         // i64
    std::uint64_t,        // u64
    double,               // f64
    std::filesystem::path // module
>;

enum class type_fundamental : std::uint8_t
{
    null_type,
    bool_type,
    char_type,
    i32_type,
    i64_type,
    u64_type,
    f64_type,
    module_type,
};

struct type_struct
{
    std::string            name;
    std::filesystem::path  module;
    std::vector<type_name> templates;

    auto to_hash() const { return hash(name, module, templates); }
    auto operator==(const type_struct&) const -> bool = default;
};

struct type_array
{
    value_ptr<type_name> inner_type;
    std::size_t          count;

    auto to_hash() const { return hash(inner_type, count); }
    auto operator==(const type_array&) const -> bool = default;
};

struct type_ptr
{
    value_ptr<type_name> inner_type;

    auto to_hash() const { return hash(inner_type); }
    auto operator==(const type_ptr&) const -> bool = default;
};

struct type_span
{
    value_ptr<type_name> inner_type;

    auto to_hash() const { return hash(inner_type); }
    auto operator==(const type_span&) const -> bool = default;
};

struct type_function_ptr
{
    std::vector<type_name> param_types;
    value_ptr<type_name>   return_type;

    auto to_hash() const { return hash(param_types, return_type); }
    auto operator==(const type_function_ptr&) const -> bool = default;
};

struct type_builtin
{
    std::string            name; // for printing only
    std::size_t            id;
    std::vector<type_name> args;
    value_ptr<type_name>   return_type;

    auto to_hash() const { return hash(name, id, args, return_type); }
    auto operator==(const type_builtin&) const -> bool = default;
};

struct type_bound_method
{
    std::size_t            id;
    std::vector<type_name> param_types;
    value_ptr<type_name>   return_type;

    auto to_hash() const { return hash(id, param_types, return_type); }
    auto operator==(const type_bound_method&) const -> bool = default;
};

struct type_bound_method_template
{
    std::filesystem::path    module;
    type_struct              struct_name;
    std::string              name;

    auto to_hash() const { return hash(module, struct_name, name); }
    auto operator==(const type_bound_method_template&) const -> bool = default;
};

struct type_arena
{
    auto to_hash() const { return hash(0); }
    auto operator==(const type_arena&) const -> bool = default;
};

struct type_type
{
    value_ptr<type_name> type_val;

    auto to_hash() const { return hash(type_val); }
    auto operator==(const type_type&) const -> bool = default;
};

struct type_function
{
    std::size_t            id;
    std::vector<type_name> param_types;
    value_ptr<type_name>   return_type;

    auto to_pointer() const -> type_name;
    auto to_hash() const { return hash(id, param_types, return_type); }
    auto to_bound_method() -> type_bound_method { return {id, param_types, return_type}; }
    auto operator==(const type_function&) const -> bool = default;
};

struct type_function_template
{
    std::filesystem::path    module;
    type_struct              struct_name;
    std::string              name;

    auto to_hash() const { return hash(module, struct_name, name); }
    auto to_string() const -> std::string;
    auto operator==(const type_function_template&) const -> bool = default;
};

struct type_struct_template
{
    std::filesystem::path module;
    std::string           name;

    auto to_hash() const { return hash(module, name); }
    auto operator==(const type_struct_template&) const -> bool = default;
};

// Only used during template argument type deduction
struct type_placeholder
{
    std::string name;

    auto to_hash() const { return hash(name); }
    auto operator==(const type_placeholder&) const -> bool = default;
};

auto to_string(const type_name& type) -> std::string;
auto to_string(type_fundamental t) -> std::string;
auto to_string(const type_array& type) -> std::string;
auto to_string(const type_ptr& type) -> std::string;
auto to_string(const type_span& type) -> std::string;
auto to_string(const type_struct& type) -> std::string;
auto to_string(const type_function_ptr& type) -> std::string;
auto to_string(const type_builtin& type) -> std::string;
auto to_string(const type_bound_method& type) -> std::string;
auto to_string(const type_bound_method_template& type) -> std::string;
auto to_string(const type_arena& type) -> std::string;
auto to_string(const type_type& type) -> std::string;
auto to_string(const type_function& type) -> std::string;
auto to_string(const type_function_template& type) -> std::string;
auto to_string(const type_struct_template& type) -> std::string;
auto to_string(const type_placeholder& type) -> std::string;

struct type_name : public std::variant<
    type_fundamental,
    type_struct,
    type_array,
    type_ptr,
    type_span,
    type_arena,
    type_function_ptr,
    type_bound_method,
    type_bound_method_template,
    type_builtin,
    type_type,
    type_function,
    type_function_template,
    type_struct_template,
    type_placeholder>
{
    using variant::variant;
    type_name(const type_name&) = default;
    
    bool is_const = false;

    [[nodiscard]] auto add_ptr() const -> type_name;
    [[nodiscard]] auto remove_ptr() const -> type_name;
 
    [[nodiscard]] auto add_const() const -> type_name;
    [[nodiscard]] auto remove_const() const -> type_name;

    [[nodiscard]] auto add_array(std::size_t size) const -> type_name;
    [[nodiscard]] auto remove_array() const -> type_name;

    [[nodiscard]] auto add_span() const -> type_name;
    [[nodiscard]] auto remove_span() const -> type_name;

    template <typename T> auto is()     const -> bool     { return std::holds_alternative<T>(*this); }
    template <typename T> auto as()     const -> const T& { return std::get<T>(*this); }
    template <typename T> auto get_if() const -> const T* { return std::get_if<T>(this); }

    auto to_hash() const -> std::size_t {
        return std::visit([](const auto& obj) { return hash(obj); }, *this);
    }

    auto to_string() const -> std::string {
        const auto inner = std::visit([](const auto& obj) { return anzu::to_string(obj); }, *this);
        return std::format("{}{}", inner, is_const ? " const" : "");
    }
};

// Used for resolving template types. In the future could also be used for type aliases
using template_map = std::unordered_map<std::string, type_name>;

auto null_type() -> type_name;
auto bool_type() -> type_name;
auto char_type() -> type_name;
auto i32_type() -> type_name;
auto i64_type() -> type_name;
auto u64_type() -> type_name;
auto f64_type() -> type_name;
auto module_type() -> type_name;
auto arena_type() -> type_name;
auto string_literal_type() -> type_name;

// Extracts the single inner type of the given t. Undefined if the given t is not a compound
// type with a single subtype.
auto inner_type(const type_name& t) -> type_name;

}

template <>
struct std::formatter<std::byte> : std::formatter<std::string>
{
    auto format(std::byte b, auto& ctx) const {
        const auto str = std::format("{:X}", static_cast<unsigned char>(b));
        return std::formatter<std::string>::format(str, ctx);
    }
};