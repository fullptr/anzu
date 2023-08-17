#include "object.hpp"
#include "token.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <cassert>
#include <algorithm>
#include <ranges>
#include <string_view>

namespace anzu {
namespace {

static constexpr auto PTR_SIZE = std::size_t{8};

auto format_error(const std::string& str) -> void
{
    anzu::print("format error: could not format special chars in '{}'\n", str);
    std::exit(1);
}

}

auto to_string(const type_name& type) -> std::string
{
    return std::visit([](const auto& t) { return ::anzu::to_string(t); }, type);
}

auto to_string(const type_struct& type) -> std::string
{
    return type.name;
}

auto to_string(const type_array& type) -> std::string
{
    return std::format("{}[{}]", to_string(*type.inner_type), type.count);
}

auto to_string(const type_ptr& type) -> std::string
{
    return std::format("&{}", to_string(*type.inner_type));
}

auto to_string(const type_span& type) -> std::string
{
    return std::format("{}[]", to_string(*type.inner_type));
}

auto to_string(const type_function_ptr& type) -> std::string
{
    return std::format("({}) -> {}", format_comma_separated(type.param_types), *type.return_type);
}

auto to_string(const type_reference& type) -> std::string
{
    return std::format("{}~", to_string(*type.inner_type));
}

auto hash(const type_name& type) -> std::size_t
{
    return std::visit([](const auto& t) { return hash(t); }, type);
}

auto hash(const type_struct& type) -> std::size_t
{
    return std::hash<std::string>{}(type.name);
}

auto hash(const type_array& type) -> std::size_t
{
    return hash(*type.inner_type) ^ std::hash<std::size_t>{}(type.count);
}

auto hash(const type_ptr& type) -> std::size_t
{
    static const auto base = std::hash<std::string_view>{}("type_ptr");
    return hash(*type.inner_type) ^ base;
}

auto hash(const type_span& type) -> std::size_t
{
    static const auto base = std::hash<std::string_view>{}("type_span");
    return hash(*type.inner_type) ^ base;
}

auto hash(const type_function_ptr& type) -> std::size_t
{
    auto val = hash(*type.return_type);
    for (const auto& param : type.param_types) {
        val ^= hash(param);
    }
    return val;
}

auto hash(const type_reference& type) -> std::size_t
{
    static const auto base = std::hash<std::string_view>{}("type_reference");
    return hash(*type.inner_type) ^ base;
}

auto i32_type() -> type_name
{
    return {type_struct{ .name = std::string{i32_sv} }};
}

auto i64_type() -> type_name
{
    return {type_struct{ .name = std::string{i64_sv} }};
}

auto u64_type() -> type_name
{
    return {type_struct{ .name = std::string{u64_sv} }};
}

auto char_type() -> type_name
{
    return {type_struct{ .name = std::string{char_sv} }};
}

auto f64_type() -> type_name
{
    return {type_struct{ .name = std::string{f64_sv} }};
}

auto bool_type() -> type_name
{
    return {type_struct{ .name = std::string{bool_sv} }};
}

auto null_type() -> type_name
{
    return {type_struct{ .name = std::string{null_sv} }};
}

auto make_type(const std::string& name) -> type_name
{
    return { type_struct{ .name=name } };
}

auto concrete_array_type(const type_name& t, std::size_t size) -> type_name
{
    return {type_array{ .inner_type = { t }, .count = size }};
}

auto is_array_type(const type_name& t) -> bool
{
    return std::holds_alternative<type_array>(t);
}

auto concrete_ptr_type(const type_name& t) -> type_name
{
    return {type_ptr{ .inner_type = { t } }};
}

auto is_ptr_type(const type_name& t) -> bool
{
    return std::holds_alternative<type_ptr>(t);
}

auto concrete_span_type(const type_name& t) -> type_name
{
    return {type_span{ .inner_type = { t } }};
}

auto is_span_type(const type_name& t) -> bool
{
    return std::holds_alternative<type_span>(t);
}

auto is_function_ptr_type(const type_name& t) -> bool
{
    return std::holds_alternative<type_function_ptr>(t);
}

auto concrete_reference_type(const type_name& t) -> type_name
{
    return {type_reference{ .inner_type = { t } }};
}

auto is_reference_type(const type_name& t) -> bool
{
    return std::holds_alternative<type_reference>(t);
}

auto inner_type(const type_name& t) -> type_name
{
    if (is_array_type(t)) {
        return *std::get<type_array>(t).inner_type;
    }
    if (is_ptr_type(t)) {
        return *std::get<type_ptr>(t).inner_type;
    }
    if (is_span_type(t)) {
        return *std::get<type_span>(t).inner_type; 
    }
    if (is_reference_type(t)) {
        return *std::get<type_reference>(t).inner_type;
    }
    print("COMPILER ERROR: Tried to get the inner type of an invalid type category, "
          "can only get the inner type for arrays, pointers, spans and references\n");
    std::exit(1);
    return {};
}

auto array_length(const type_name& t) -> std::size_t
{
    if (is_array_type(t)) {
        return std::get<type_array>(t).count;
    }
    print("COMPILER ERROR: Tried to get length of a non-array type\n");
    std::exit(1);
    return {};
}

auto size_of_ptr() -> std::size_t
{
    return PTR_SIZE;
}

auto size_of_span() -> std::size_t
{
    return 2 * PTR_SIZE; // actually a pointer + a size, but they are both 8 bytes
}

auto size_of_reference() -> std::size_t
{
    return PTR_SIZE;
}

auto is_type_fundamental(const type_name& type) -> bool
{
    return type == i32_type()
        || type == i64_type()
        || type == u64_type()
        || type == f64_type()
        || type == char_type()
        || type == bool_type()
        || type == null_type();
}

auto is_type_trivially_copyable(const type_name& type) -> bool
{
    // TODO: Allow for trivially copyable user types
    //   ie- classes with default copy/assign and all trivially copyable members
    return is_type_fundamental(type)
        || is_ptr_type(type)
        || is_function_ptr_type(type)
        || (is_array_type(type) && is_type_trivially_copyable(inner_type(type)))
        || is_span_type(type);
}

auto is_type_convertible_to(const type_name& type, const type_name& expected) -> bool
{
    return type == expected
        || (is_reference_type(type) && inner_type(type) == expected)
        || (is_reference_type(expected) && inner_type(expected) == type)
        || (is_array_type(type) && is_span_type(expected) && inner_type(type) == inner_type(expected));
}

// Checks if the set of given args is convertible to the signature for a function.
// Type A is convertible to B is A == ref B or B == ref A. TODO: Consider value categories,
// rvalues should not be bindable to references
auto are_types_convertible_to(const std::vector<type_name>& args,
                       const std::vector<type_name>& actuals) -> bool
{
    if (args.size() != actuals.size()) return false;
    for (std::size_t i = 0; i != args.size(); ++i) {
        if (!is_type_convertible_to(args[i], actuals[i])) {
            return false;
        }
    }
    return true;
}

auto remove_reference(const type_name& type) -> type_name
{
    return is_reference_type(type) ? inner_type(type) : type;
}

auto type_store::add(const type_name& name, const type_fields& fields) -> bool
{
    if (d_classes.contains(name)) {
        return false;
    }
    d_classes.emplace(name, fields);
    return true;
}

auto type_store::contains(const type_name& type) const -> bool
{
    return d_classes.contains(type)
        || is_type_fundamental(type)
        || is_array_type(type)
        || is_ptr_type(type)
        || is_function_ptr_type(type)
        || is_span_type(type)
        || is_reference_type(type); // is just a pointer
}

// TODO: Refactor this mess
auto type_store::size_of(const type_name& type) const -> std::size_t
{
    return std::visit(overloaded{
        [&](const type_struct& t) -> std::size_t {
            if (type == null_type() || type == char_type() || type == bool_type()) {
                return 1;
            }
            
            if (type == i32_type()) {
                return 4;
            }

            if (type == i64_type() || type == f64_type() || type == u64_type()) {
                return 8;
            }

            if (!d_classes.contains(type)) {
                print("unknown type '{}'\n", type);
                std::exit(1);
            }
            auto size = std::size_t{0};
            for (const auto& field : fields_of(type)) {
                size += size_of(field.type);
            }
            return size;
        },
        [&](const type_array& t) {
            return size_of(*t.inner_type) * t.count;
        },
        [](const type_ptr&) {
            return size_of_ptr();
        },
        [](const type_span&) {
            return size_of_span();
        },
        [](const type_function_ptr&) {
            return size_of_ptr();
        },
        [](const type_reference&) {
            return size_of_reference();
        }
    }, type);
}

auto type_store::fields_of(const type_name& t) const -> type_fields
{
    if (auto it = d_classes.find(t); it != d_classes.end()) {
        return it->second.fields;
    }
    return {};
}

}
