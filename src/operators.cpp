#include "operators.hpp"

namespace anzu {
namespace {

template <typename T>
auto get_back(std::vector<block>& mem, std::size_t index) -> T&
{
    return std::get<std::remove_cvref_t<T>>(mem[mem.size() - index - 1]);
}

template <typename Lhs, typename Rhs>
auto back_two(std::vector<block>& mem)
{
    const auto rhs = get_back<Lhs>(mem, 0);
    const auto lhs = get_back<Rhs>(mem, 1);
    return std::pair{lhs, rhs};
}

template <typename T>
auto pop_and_emplace(std::vector<block>& mem, T&& val)
{
    mem.pop_back();
    mem.back().emplace<T>(val);
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
            return bin_op_info{ .operator_func = int_add_int, .result_type = type };
        } else if (desc.op == tk_sub) {
            return bin_op_info{ .operator_func = int_sub_int, .result_type = type };
        } else if (desc.op == tk_mul) {
            return bin_op_info{ .operator_func = int_mul_int, .result_type = type };
        } else if (desc.op == tk_div) {
            return bin_op_info{ .operator_func = int_div_int, .result_type = type };
        } else if (desc.op == tk_mod) {
            return bin_op_info{ .operator_func = int_mod_int, .result_type = type };
        } else if (desc.op == "<") {
            return bin_op_info{ .operator_func = int_lt_int, .result_type = bool_type() };
        } else if (desc.op == "<=") {
            return bin_op_info{ .operator_func = int_le_int, .result_type = bool_type() };
        } else if (desc.op == ">") {
            return bin_op_info{ .operator_func = int_gt_int, .result_type = bool_type() };
        } else if (desc.op == ">=") {
            return bin_op_info{ .operator_func = int_ge_int, .result_type = bool_type() };
        } else if (desc.op == "==") {
            return bin_op_info{ .operator_func = int_eq_int, .result_type = bool_type() };
        } else if (desc.op == "!=") {
            return bin_op_info{ .operator_func = int_ne_int, .result_type = bool_type() };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == "==") {
            return bin_op_info{ .operator_func = bool_eq_bool, .result_type = type };
        } else if (desc.op == "!=") {
            return bin_op_info{ .operator_func = bool_ne_bool, .result_type = type };
        } else if (desc.op == "&&") {
            return bin_op_info{ .operator_func = bool_and_bool, .result_type = type };
        } else if (desc.op == "||") {
            return bin_op_info{ .operator_func = bool_or_bool, .result_type = type };
        }
    }
    else if (type == str_type()) {
        if (desc.op == "+") {
            return bin_op_info{ .operator_func = str_add_str, .result_type = type };
        } else if (desc.op == "==") {
            return bin_op_info{ .operator_func = str_eq_str, .result_type = bool_type() };
        } else if (desc.op == "!=") {
            return bin_op_info{ .operator_func = str_ne_str, .result_type = bool_type() };
        }
    }

    return std::nullopt;
}

void int_add_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_int>(mem, lhs + rhs);
}

void int_sub_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_int>(mem, lhs - rhs);
}

void int_mul_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_int>(mem, lhs * rhs);
}

void int_div_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    if (rhs == 0) {
        anzu::print("Runtime Error: division by zero\n");
        std::exit(1);
    }
    pop_and_emplace<block_int>(mem, lhs / rhs);
}

void int_mod_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_int>(mem, lhs % rhs);
}

void int_lt_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_bool>(mem, lhs < rhs);
}

void int_le_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_bool>(mem, lhs <= rhs);
}

void int_gt_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_bool>(mem, lhs > rhs);
}

void int_ge_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_bool>(mem, lhs >= rhs);
}

void int_eq_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_bool>(mem, lhs == rhs);
}

void int_ne_int(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_int, block_int>(mem);
    pop_and_emplace<block_bool>(mem, lhs != rhs);
}

void bool_eq_bool(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_bool, block_bool>(mem);
    pop_and_emplace<block_bool>(mem, lhs == rhs);
}

void bool_ne_bool(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_bool, block_bool>(mem);
    pop_and_emplace<block_bool>(mem, lhs != rhs);
}

void bool_and_bool(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_bool, block_bool>(mem);
    pop_and_emplace<block_bool>(mem, lhs && rhs);
}

void bool_or_bool(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_bool, block_bool>(mem);
    pop_and_emplace<block_bool>(mem, lhs || rhs);
}

void str_add_str(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_str&, block_str&>(mem);
    pop_and_emplace<block_str>(mem, lhs + rhs);
}

void str_eq_str(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_str&, block_str&>(mem);
    pop_and_emplace<block_bool>(mem, lhs == rhs);
}

void str_ne_str(std::vector<block>& mem)
{
    const auto [lhs, rhs] = back_two<block_str&, block_str&>(mem);
    pop_and_emplace<block_bool>(mem, lhs != rhs);
}

}