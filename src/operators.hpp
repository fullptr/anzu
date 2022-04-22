#pragma once
#include "object.hpp"
#include "functions.hpp"

#include <functional>
#include <optional>
#include <vector>

namespace anzu {

struct binary_op_description
{
    std::string op;
    type_name   lhs;
    type_name   rhs;
};

struct binary_op_info
{
    builtin_function operator_func;
    type_name        result_type;
};

auto resolve_binary_op(
    const type_store& types, const binary_op_description& desc
) -> std::optional<binary_op_info>;

struct unary_op_description
{
    std::string op;
    type_name   type;
};

struct unary_op_info
{
    builtin_function operator_func;
    type_name        result_type;
};

auto resolve_unary_op(const unary_op_description& desc) -> std::optional<unary_op_info>;

}