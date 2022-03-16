#include "operators.hpp"

#include <functional>

namespace anzu {
namespace {

template <typename T>
auto get_back(std::vector<block>& mem, std::size_t index) -> T&
{
    return std::get<std::remove_cvref_t<T>>(mem[mem.size() - index - 1]);
}

template <typename Lhs, typename Op, typename Rhs>
auto bin_op(std::vector<block>& mem)
{
    const auto op = Op{};
    const auto rhs = get_back<Lhs>(mem, 0);
    const auto lhs = get_back<Rhs>(mem, 1);
    mem.pop_back();
    mem.back().emplace<decltype(op(lhs, rhs))>(op(lhs, rhs));
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
            return bin_op_info{ bin_op<block_int, std::plus<block_int>, block_int>, type };
        } else if (desc.op == tk_sub) {
            return bin_op_info{ bin_op<block_int, std::minus<block_int>, block_int>, type };
        } else if (desc.op == tk_mul) {
            return bin_op_info{ bin_op<block_int, std::multiplies<block_int>, block_int>, type };
        } else if (desc.op == tk_div) {
            return bin_op_info{ bin_op<block_int, std::divides<block_int>, block_int>, type };
        } else if (desc.op == tk_mod) {
            return bin_op_info{ bin_op<block_int, std::modulus<block_int>, block_int>, type };
        } else if (desc.op == "<") {
            return bin_op_info{ bin_op<block_int, std::less<block_int>, block_int>, bool_type() };
        } else if (desc.op == "<=") {
            return bin_op_info{ bin_op<block_int, std::less_equal<block_int>, block_int>, bool_type() };
        } else if (desc.op == ">") {
            return bin_op_info{ bin_op<block_int, std::greater<block_int>, block_int>, bool_type() };
        } else if (desc.op == ">=") {
            return bin_op_info{ bin_op<block_int, std::greater_equal<block_int>, block_int>, bool_type() };
        } else if (desc.op == "==") {
            return bin_op_info{ bin_op<block_int, std::equal_to<block_int>, block_int>, bool_type() };
        } else if (desc.op == "!=") {
            return bin_op_info{ bin_op<block_int, std::not_equal_to<block_int>, block_int>, bool_type() };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == "==") {
            return bin_op_info{ bin_op<block_bool, std::equal_to<block_bool>, block_bool>, type };
        } else if (desc.op == "!=") {
            return bin_op_info{ bin_op<block_bool, std::not_equal_to<block_bool>, block_bool>, type };
        } else if (desc.op == "&&") {
            return bin_op_info{ bin_op<block_bool, std::logical_and<block_bool>, block_bool>, type };
        } else if (desc.op == "||") {
            return bin_op_info{ bin_op<block_bool, std::logical_or<block_bool>, block_bool>, type };
        }
    }
    else if (type == str_type()) {
        if (desc.op == "+") {
            return bin_op_info{ bin_op<block_str, std::plus<block_str>, block_str>, type };
        } else if (desc.op == "==") {
            return bin_op_info{ bin_op<block_str, std::equal_to<block_str>, block_str>, bool_type() };
        } else if (desc.op == "!=") {
            return bin_op_info{ bin_op<block_str, std::not_equal_to<block_str>, block_str>, bool_type() };
        }
    }

    return std::nullopt;
}

}