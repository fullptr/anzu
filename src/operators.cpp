#include "operators.hpp"
#include "object.hpp"

#include <algorithm>
#include <functional>

namespace anzu {
namespace {

template <typename T>
auto push_value(std::vector<std::byte>& mem, const T& value) -> void
{
    for (const auto& b : std::bit_cast<std::array<std::byte, sizeof(T)>>(value)) {
        mem.push_back(b);
    }
}

template <typename T>
auto pop_value(std::vector<std::byte>& mem) -> T
{
    auto ret = T{};
    std::memcpy(&ret, &mem[mem.size() - sizeof(T)], sizeof(T));
    mem.resize(mem.size() - sizeof(T));
    return ret;
}

template <typename Type, template <typename> typename Op>
auto bin_op(std::vector<std::byte>& mem) -> void
{
    static constexpr auto op = Op<Type>{};
    const auto rhs = pop_value<Type>(mem);
    const auto lhs = pop_value<Type>(mem);
    push_value(mem, op(lhs, rhs));
}

// Top of stack: [ptr], [size], [offset]
// offset is popped, size stays the same, ptr is modified
auto ptr_addition(std::vector<std::byte>& mem)
{
    const auto offset = pop_value<std::uint64_t>(mem);
    const auto size = pop_value<std::uint64_t>(mem);
    const auto ptr = pop_value<std::uint64_t>(mem);
    push_value(mem, ptr + offset * size);
    push_value(mem, size);
}

template <typename Type, template <typename> typename Op>
auto unary_op(std::vector<std::byte>& mem)
{
    static constexpr auto op = Op<Type>{};
    const auto obj = pop_value<Type>(mem);
    push_value(mem, op(obj));
}

}

auto resolve_binary_op(const binary_op_description& desc) -> std::optional<binary_op_info>
{
    if (is_ptr_type(desc.lhs) && desc.rhs == u64_type()) {
        return binary_op_info{ ptr_addition, desc.lhs };
    }

    if (desc.lhs != desc.rhs) {
        return std::nullopt;
    }

    const auto& type = desc.lhs;
    
    if (is_list_type(type)) { // No support for having these in bin ops.
        return std::nullopt;
    }

    if (type == i32_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op<std::int32_t, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op<std::int32_t, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op<std::int32_t, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op<std::int32_t, std::divides>, type };
        } else if (desc.op == tk_mod) {
            return binary_op_info{ bin_op<std::int32_t, std::modulus>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op<std::int32_t, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op<std::int32_t, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op<std::int32_t, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op<std::int32_t, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op<std::int32_t, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op<std::int32_t, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == i64_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op<std::int64_t, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op<std::int64_t, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op<std::int64_t, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op<std::int64_t, std::divides>, type };
        } else if (desc.op == tk_mod) {
            return binary_op_info{ bin_op<std::int64_t, std::modulus>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op<std::int64_t, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op<std::int64_t, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op<std::int64_t, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op<std::int64_t, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op<std::int64_t, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op<std::int64_t, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == u64_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op<std::uint64_t, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op<std::uint64_t, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op<std::uint64_t, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op<std::uint64_t, std::divides>, type };
        } else if (desc.op == tk_mod) {
            return binary_op_info{ bin_op<std::uint64_t, std::modulus>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op<std::uint64_t, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op<std::uint64_t, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op<std::uint64_t, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op<std::uint64_t, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op<std::uint64_t, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op<std::uint64_t, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == f64_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op<double, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op<double, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op<double, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op<double, std::divides>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op<double, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op<double, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op<double, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op<double, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op<double, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op<double, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == tk_and) {
            return binary_op_info{ bin_op<bool, std::logical_and>, type };
        } else if (desc.op == tk_or) {
            return binary_op_info{ bin_op<bool, std::logical_or>, type };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op<bool, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op<bool, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == char_type()) {
        if (desc.op == tk_eq) {
            return binary_op_info{ bin_op<char, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op<char, std::equal_to>, bool_type() };
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