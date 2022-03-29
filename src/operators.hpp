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

struct unary_op_description
{
    std::string op;
    type_name   type;
};

struct unary_op_info
{
    builtin_mem_op operator_func;
    type_name      result_type;
};

auto resolve_unary_op(const unary_op_description& desc) -> std::optional<unary_op_info>;

}