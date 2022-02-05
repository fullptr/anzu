#pragma once
#include <string>
#include <unordered_set>

#include "token.hpp"

namespace anzu {

// A class for registering and storing types. This will only store builtin types to
// begin with, but will be extended to allow for custom types.
class type_store
{
    std::unordered_set<std::string> d_types;

public:
    type_store();
    auto is_valid_type(const std::string& type) -> bool;
    auto is_valid_type(const anzu::token& token) -> bool;
};

}