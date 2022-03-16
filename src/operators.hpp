#pragma once
#include "object.hpp"
#include "type.hpp"

#include <functional>
#include <optional>
#include <vector>

namespace anzu {

using builtin_mem_op = std::function<void(std::vector<block>& memory)>;

struct bin_op_description
{
    std::string op;
    type_name   lhs;
    type_name   rhs;
};

struct bin_op_info
{
    builtin_mem_op operator_func;
    type_name      result_type;
};

auto resolve_bin_op(const bin_op_description& desc) -> std::optional<bin_op_info>;

void int_add_int(std::vector<block>& mem);
void int_sub_int(std::vector<block>& mem);
void int_mul_int(std::vector<block>& mem);
void int_div_int(std::vector<block>& mem);
void int_mod_int(std::vector<block>& mem);

void int_lt_int(std::vector<block>& mem);
void int_le_int(std::vector<block>& mem);
void int_gt_int(std::vector<block>& mem);
void int_ge_int(std::vector<block>& mem);
void int_eq_int(std::vector<block>& mem);
void int_ne_int(std::vector<block>& mem);

void bool_eq_bool(std::vector<block>& mem);
void bool_ne_bool(std::vector<block>& mem);
void bool_and_bool(std::vector<block>& mem);
void bool_or_bool(std::vector<block>& mem);

void str_add_str(std::vector<block>& mem);
void str_eq_str(std::vector<block>& mem);
void str_ne_str(std::vector<block>& mem);

}