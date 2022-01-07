#pragma once
#include <string>
#include <variant>

namespace anzu {

using object = std::variant<int, bool>;

bool is_literal(const std::string& token);
anzu::object parse_literal(const std::string& token);

}