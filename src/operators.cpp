#include "operators.hpp"
#include "object.hpp"
#include "utility/memory.hpp"

#include <algorithm>
#include <functional>

namespace anzu {
namespace {

template <typename T>
auto to_type_name() -> type_name
{
    if constexpr (std::is_same_v<T, std::int32_t>) {
        return i32_type();
    } else if constexpr (std::is_same_v<T, std::int64_t>) {
        return i64_type();
    } else if constexpr (std::is_same_v<T, std::uint64_t>) {
        return u64_type();
    } else if constexpr (std::is_same_v<T, double>) {
        return f64_type();
    } else {
        static_assert(false);
    }
}

template <typename Type, template <typename> typename Op>
auto bin_op(std::vector<std::byte>& mem) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto rhs = pop_value<Type>(mem);
    const auto lhs = pop_value<Type>(mem);
    push_value(mem, op(lhs, rhs));
}

auto ptr_addition(std::size_t type_size)
{
    return [=](std::vector<std::byte>& mem) {
        const auto offset = pop_value<std::uint64_t>(mem);
        const auto ptr = pop_value<std::uint64_t>(mem);
        push_value(mem, ptr + offset * type_size);
    };
}

template <typename Type, template <typename> typename Op>
auto unary_op(std::vector<std::byte>& mem)
{
    static constexpr auto op = Op<Type>{};
    const auto obj = pop_value<Type>(mem);
    push_value(mem, op(obj));
}

template <typename T>
auto resolve_arithmetic_binary_op(std::string_view op) -> std::optional<binary_op_info>
{
    const auto type = to_type_name<T>();
    if (op == tk_add) {
        return binary_op_info{ bin_op<T, std::plus>, type };
    } else if (op == tk_sub) {
        return binary_op_info{ bin_op<T, std::minus>, type };
    } else if (op == tk_mul) {
        return binary_op_info{ bin_op<T, std::multiplies>, type };
    } else if (op == tk_div) {
        return binary_op_info{ bin_op<T, std::divides>, type };
    }
    return std::nullopt;
}

template <typename T>
auto resolve_equality_binary_op(std::string_view op) -> std::optional<binary_op_info>
{
    if (op == tk_eq) {
        return binary_op_info{ bin_op<T, std::equal_to>, bool_type() };
    } else if (op == tk_ne) {
        return binary_op_info{ bin_op<T, std::not_equal_to>, bool_type() };
    }
    return std::nullopt;
}

template <typename T>
auto resolve_comparison_binary_op(std::string_view op) -> std::optional<binary_op_info>
{
    if (op == tk_lt) {
        return binary_op_info{ bin_op<T, std::less>, bool_type() };
    } else if (op == tk_le) {
        return binary_op_info{ bin_op<T, std::less_equal>, bool_type() };
    } else if (op == tk_gt) {
        return binary_op_info{ bin_op<T, std::greater>, bool_type() };
    } else if (op == tk_ge) {
        return binary_op_info{ bin_op<T, std::greater_equal>, bool_type() };
    }
    return std::nullopt;
}

template <typename T>
auto resolve_numerical_binary_op(std::string_view op) -> std::optional<binary_op_info>
{
    if (auto bin_op = resolve_arithmetic_binary_op<T>(op)) {
        return bin_op.value();
    }
    if (auto bin_op = resolve_equality_binary_op<T>(op)) {
        return bin_op.value();
    }
    if (auto bin_op = resolve_comparison_binary_op<T>(op)) {
        return bin_op.value();
    }
    if constexpr (!std::is_floating_point_v<T>) {
        if (op == tk_mod) {
            return binary_op_info{ bin_op<T, std::modulus>, to_type_name<T>() };
        }
    }
    return std::nullopt;
}

}

auto resolve_binary_op(
    const type_store& types, const binary_op_description& desc
) -> std::optional<binary_op_info>
{
    if (is_ptr_type(desc.lhs) && desc.rhs == u64_type()) {
        return binary_op_info{ ptr_addition(types.size_of(desc.lhs)), desc.lhs };
    }

    if (desc.lhs != desc.rhs) {
        return std::nullopt;
    }

    const auto& type = desc.lhs;
    
    if (is_list_type(type)) { // No support for having these in bin ops.
        return std::nullopt;
    }

    if (type == i32_type()) {
        return resolve_numerical_binary_op<std::int32_t>(desc.op);
    }
    else if (type == i64_type()) {
        return resolve_numerical_binary_op<std::int64_t>(desc.op);
    }
    else if (type == u64_type()) {
        return resolve_numerical_binary_op<std::uint64_t>(desc.op);
    }
    else if (type == f64_type()) {
        return resolve_numerical_binary_op<double>(desc.op);
    }
    else if (type == bool_type()) {
        if (desc.op == tk_and) {
            return binary_op_info{ bin_op<bool, std::logical_and>, type };
        }
        if (desc.op == tk_or) {
            return binary_op_info{ bin_op<bool, std::logical_or>, type };
        }
        
        if (auto op = resolve_equality_binary_op<bool>(desc.op)) {
            return op.value();
        }
    }
    else if (type == char_type()) {
        if (auto op = resolve_equality_binary_op<char>(desc.op)) {
            return op.value();
        }
    }
    return std::nullopt;
}

auto resolve_unary_op(const unary_op_description& desc) -> std::optional<unary_op_info>
{
    const auto& type = desc.type;
    if (type == i32_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op<std::int32_t, std::negate>, type };
        }
    }
    else if (type == i64_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op<std::int64_t, std::negate>, type };
        }
    }
    else if (type == f64_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op<double, std::negate>, type };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == tk_bang) {
            return unary_op_info{ unary_op<bool, std::logical_not>, type };
        }
    }
    return std::nullopt;
}

}