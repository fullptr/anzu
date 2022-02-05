#include "types.hpp"

namespace anzu {

type_store::type_store()
    : d_types{"int", "bool", "str", "list", "null", "any"}
{   
}

auto type_store::is_valid_type(const std::string& type) -> bool
{
    return d_types.contains(type);
}

auto type_store::is_valid_type(const anzu::token& token) -> bool
{
    return token.type == token_type::name && is_valid_type(token.text);
}

}