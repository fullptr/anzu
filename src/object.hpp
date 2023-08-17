#pragma once
#include <format>
#include <memory>
#include <string>
#include <variant>
#include <unordered_map>
#include <vector>

#include "utility/print.hpp"
#include "utility/value_ptr.hpp"

namespace anzu {

// Want these to be equivalent since we want uints available in the runtime but we also want
// to use it as indexes into C++ vectors which use size_t.
static_assert(std::is_same_v<std::uint64_t, std::size_t>);

struct type_name;

enum class fundamental : std::uint8_t
{
    null_type,
    bool_type,
    char_type,
    i32_type,
    i64_type,
    u64_type,
    f64_type,
};

struct type_fundamental
{
    fundamental type;
    auto operator==(const type_fundamental&) const -> bool = default;
};

struct type_struct
{
    std::string name;
    auto operator==(const type_struct&) const -> bool = default;
};

struct type_array
{
    value_ptr<type_name> inner_type;
    std::size_t          count;
    auto operator==(const type_array&) const -> bool = default;
};

struct type_ptr
{
    value_ptr<type_name> inner_type;
    auto operator==(const type_ptr&) const -> bool = default;
};

struct type_span
{
    value_ptr<type_name> inner_type;
    auto operator==(const type_span&) const -> bool = default;
};

struct type_function_ptr
{
    std::vector<type_name> param_types;
    value_ptr<type_name>   return_type;
    auto operator==(const type_function_ptr&) const -> bool = default;
};

struct type_reference
{
    value_ptr<type_name> inner_type;
    auto operator==(const type_reference&) const -> bool = default;
};

struct type_name : public std::variant<
    type_fundamental,
    type_struct,
    type_array,
    type_ptr,
    type_span,
    type_function_ptr,
    type_reference>
{
    using variant::variant;
};

using type_names = std::vector<type_name>;

struct field
{
    std::string name;
    type_name   type;
    auto operator==(const field&) const -> bool = default;
};
using type_fields = std::vector<field>;

struct type_info
{
    type_fields fields;
};

auto hash(const type_name& type) -> std::size_t;
auto hash(type_fundamental type) -> std::size_t;
auto hash(const type_struct& type) -> std::size_t;
auto hash(const type_array& type) -> std::size_t;
auto hash(const type_ptr& type) -> std::size_t;
auto hash(const type_span& type) -> std::size_t;
auto hash(const type_function_ptr& type) -> std::size_t;
auto hash(const type_reference& type) -> std::size_t;

auto null_type() -> type_name;
auto bool_type() -> type_name;
auto char_type() -> type_name;
auto i32_type() -> type_name;
auto i64_type() -> type_name;
auto u64_type() -> type_name;
auto f64_type() -> type_name;

auto make_type(const std::string& name) -> type_name;

auto concrete_array_type(const type_name& t, std::size_t size) -> type_name;
auto is_array_type(const type_name& t) -> bool;

auto concrete_ptr_type(const type_name& t) -> type_name;
auto is_ptr_type(const type_name& t) -> bool;

auto concrete_span_type(const type_name& t) -> type_name;
auto is_span_type(const type_name& t) -> bool;

auto is_function_ptr_type(const type_name& t) -> bool;

auto concrete_reference_type(const type_name& t) -> type_name;
auto is_reference_type(const type_name& t) -> bool;

auto size_of_ptr() -> std::size_t;
auto size_of_span() -> std::size_t;
auto size_of_reference() -> std::size_t;

// Extracts the single inner type of the given t. Undefined if the given t is not a compound
// type with a single subtype.
auto inner_type(const type_name& t) -> type_name;

// Extracts the array size of the given type. Undefined if the given t is not an array
auto array_length(const type_name& t) -> std::size_t;

auto is_type_fundamental(const type_name& type) -> bool;

auto is_type_trivially_copyable(const type_name& type) -> bool;

// Checks if the set of given args is convertible to the signature for a function.
// Type A is convertible to B is A == ref B or B == ref A.
auto is_type_convertible_to(const type_name& lhs, const type_name& rhs) -> bool;

auto are_types_convertible_to(const std::vector<type_name>& lhs,
                              const std::vector<type_name>& rhs) -> bool;

// If the type is a reference, returns the inner type, otherwise returns the type.
auto remove_reference(const type_name& type) -> type_name;

class type_store
{
    using type_hash = decltype([](const type_name& t) { return anzu::hash(t); });
    std::unordered_map<type_name, type_info, type_hash> d_classes;

public:
    auto add(const type_name& name, const type_fields& fields) -> bool;
    auto contains(const type_name& t) const -> bool;

    auto size_of(const type_name& t) const -> std::size_t;
    auto fields_of(const type_name& t) const -> type_fields;
};

auto to_string(const type_name& type) -> std::string;
auto to_string(const type_array& type) -> std::string;
auto to_string(const type_ptr& type) -> std::string;
auto to_string(const type_span& type) -> std::string;
auto to_string(const type_struct& type) -> std::string;
auto to_string(const type_function_ptr& type) -> std::string;
auto to_string(const type_reference& type) -> std::string;

// Runtime pointer helpers to determine if the pointer is in stack, heap or read-only memory.
static constexpr auto heap_bit = std::uint64_t{1} << 63;
static constexpr auto rom_bit  = std::uint64_t{1} << 62;

inline auto set_heap_bit(std::uint64_t x)   -> std::uint64_t { return x | heap_bit; }
inline auto unset_heap_bit(std::uint64_t x) -> std::uint64_t { return x & ~heap_bit; }
inline auto is_heap_ptr(std::uint64_t x)    -> bool          { return x & heap_bit; }
 
inline auto set_rom_bit(std::uint64_t x)   -> std::uint64_t { return x | rom_bit; }
inline auto unset_rom_bit(std::uint64_t x) -> std::uint64_t { return x & ~rom_bit; }
inline auto is_rom_ptr(std::uint64_t x)    -> bool          { return x & rom_bit; }

}

template <> struct std::formatter<std::byte> : std::formatter<std::string> {
    auto format(std::byte b, auto& ctx) {
        const auto str = std::format("{:X}", static_cast<unsigned char>(b));
        return std::formatter<std::string>::format(str, ctx);
    }
};

template <> struct std::formatter<anzu::type_name> : std::formatter<std::string>
{
    auto format(const anzu::type_name& type, auto& ctx) {
        return std::formatter<std::string>::format(anzu::to_string(type), ctx);
    }
};