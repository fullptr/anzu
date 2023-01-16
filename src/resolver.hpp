#pragma once
#include "object.hpp"
#include "program.hpp"

#include <optional>

namespace anzu {

struct op_info
{
    type_name return_type;
    op        op_code;
};

auto resolve_operation(const type_name& lhs, const type_name& rhs) -> std::optional<op_info>;

}