#include "types.hpp"
#include "vocabulary.hpp"

namespace anzu {

type_store::type_store()
{   
}

auto type_store::is_valid_type(const std::string& type) -> bool
{
    return anzu::is_type(type);
}

auto type_store::is_valid_type(const anzu::token& token) -> bool
{
    return token.type == token_type::keyword && anzu::is_type(token.text);
}

}