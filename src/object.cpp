#include "object.hpp"

#include <fmt/format.h>

namespace anzu {

auto is_int(const std::string& token) -> bool
{
    return token.find_first_not_of("0123456789") == std::string::npos;
}

auto to_int(const std::string& token) -> int
{
    if (!is_int(token)) {
        fmt::print("cannot convert '{}' to int\n", token);
        std::exit(1);
    }
    return std::stoi(token);
}

}