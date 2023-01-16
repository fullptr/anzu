#include "operators.hpp"
#include "object.hpp"
#include "vocabulary.hpp"
#include "program.hpp"
#include "utility/memory.hpp"

#include <algorithm>
#include <functional>

namespace anzu {
namespace {

template <typename Type, template <typename> typename Op>
auto bin_op(std::vector<std::byte>& mem) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto rhs = pop_value<Type>(mem);
    const auto lhs = pop_value<Type>(mem);
    push_value(mem, op(lhs, rhs));
}

template <typename Type, template <typename> typename Op>
auto unary_op(std::vector<std::byte>& mem)
{
    static constexpr auto op = Op<Type>{};
    const auto obj = pop_value<Type>(mem);
    push_value(mem, op(obj));
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

}

auto resolve_binary_op(
    const type_store& types, const binary_op_description& desc
) -> std::optional<binary_op_info>
{
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