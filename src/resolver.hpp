#pragma once
#include "object.hpp"
#include "program.hpp"
#include "token.hpp"

#include <optional>

namespace anzu {

struct op_info
{
    op        op_code;
    type_name return_type;
};

auto resolve_operation(const type_name& lhs, const type_name& rhs, lex_token_type op)
    -> std::optional<op_info>;

auto resolve_operation(const type_name& type, lex_token_type op)
    -> std::optional<op_info>;

}