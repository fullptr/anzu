#pragma once
#include "object.hpp"
#include "program.hpp"

#include <optional>

namespace anzu {

struct op_info
{
    op        op_code;
    type_name return_type;
};

auto resolve_operation(
    const type_name& lhs, const type_name& rhs, const std::string& operation
) -> std::optional<op_info>;

}