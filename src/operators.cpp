#include "operators.hpp"

#include <functional>

namespace anzu {
namespace {

template <typename T>
auto get_back(std::vector<block>& mem, std::size_t index) -> T&
{
    return std::get<std::remove_cvref_t<T>>(mem[mem.size() - index - 1]);
}

template <typename Type, template <typename> typename Op>
auto bin_op(std::vector<block>& mem)
{
    const auto op = Op<Type>{};
    const auto rhs = get_back<Type>(mem, 0);
    const auto lhs = get_back<Type>(mem, 1);
    mem.pop_back();
    mem.back().emplace<decltype(op(lhs, rhs))>(op(lhs, rhs));
}

template <typename Type, template <typename> typename Op>
auto unary_op(std::vector<block>& mem)
{
    const auto op = Op<Type>{};
    auto& obj = get_back<Type>(mem, 0);
    obj = op(obj);
}

}

auto resolve_bin_op(const bin_op_description& desc) -> std::optional<bin_op_info>
{
    if (desc.lhs != desc.rhs) {
        return std::nullopt;
    }
    const auto& type = desc.lhs;
    
    if (match(type, generic_list_type()).has_value()) { // No support for having these in bin ops.
        return std::nullopt;
    }

    if (type == int_type()) {
        if (desc.op == tk_add) {
            return bin_op_info{ bin_op<block_int, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return bin_op_info{ bin_op<block_int, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return bin_op_info{ bin_op<block_int, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return bin_op_info{ bin_op<block_int, std::divides>, type };
        } else if (desc.op == tk_mod) {
            return bin_op_info{ bin_op<block_int, std::modulus>, type };
        } else if (desc.op == tk_lt) {
            return bin_op_info{ bin_op<block_int, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return bin_op_info{ bin_op<block_int, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return bin_op_info{ bin_op<block_int, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return bin_op_info{ bin_op<block_int, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return bin_op_info{ bin_op<block_int, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return bin_op_info{ bin_op<block_int, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == tk_eq) {
            return bin_op_info{ bin_op<block_bool, std::equal_to>, type };
        } else if (desc.op == tk_ne) {
            return bin_op_info{ bin_op<block_bool, std::not_equal_to>, type };
        } else if (desc.op == tk_and) {
            return bin_op_info{ bin_op<block_bool, std::logical_and>, type };
        } else if (desc.op == tk_or) {
            return bin_op_info{ bin_op<block_bool, std::logical_or>, type };
        }
    }
    else if (type == str_type()) {
        if (desc.op == tk_add) {
            return bin_op_info{ bin_op<block_str, std::plus>, type };
        } else if (desc.op == tk_eq) {
            return bin_op_info{ bin_op<block_str, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return bin_op_info{ bin_op<block_str, std::not_equal_to>, bool_type() };
        }
    }

    return std::nullopt;
}

auto resolve_unary_op(const unary_op_description& desc) -> std::optional<unary_op_info>
{
    const auto& type = desc.type;
    if (type == int_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op<block_int, std::negate>, type };
        }
    }
    return std::nullopt;
}

}