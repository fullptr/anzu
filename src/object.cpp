#include "object.hpp"

#include <fmt/format.h>

namespace anzu {

bool is_literal(const std::string& token)
{
    return token == "false"
        || token == "true"
        || token.find_first_not_of("0123456789") == std::string::npos;
}

anzu::object parse_literal(const std::string& token)
{
    if (token == "true") {
        return true;
    }
    if (token == "false") {
        return false;
    }
    return parse_int(token);
    fmt::print("[Fatal] Could not parse constant: {}\n", token);
    std::exit(1);
}

int parse_int(const std::string& token)
{
    if (token.find_first_not_of("0123456789") == std::string::npos) {
        return std::stoi(token);
    }
    fmt::print("[Fatal] Could not parse constant: {}\n", token);
    std::exit(1);
}

}