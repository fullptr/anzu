#include "object.hpp"

#include <fmt/format.h>

namespace anzu {
namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

}

auto is_int(const std::string& token) -> bool
{
    return token.find_first_not_of("0123456789") == std::string::npos;
}

auto to_int(const std::string& token) -> int
{
    if (!is_int(token)) {
        fmt::print("type error: cannot convert '{}' to int\n", token);
        std::exit(1);
    }
    return std::stoi(token);
}

auto to_repr(const anzu::object& obj) -> std::string
{
    return std::visit(overloaded {
        [](int val) { return std::to_string(val); },
        [](bool val) { return std::string{val ? "true" : "false"}; },
        [](const std::string& val) {
            std::string ret{'"'};
            for (char c : val) {
                switch (c) {
                    break; case '\n': ret += "\\n";
                    break; case '\t': ret += "\\t";
                    break; case '\r': ret += "\\r";
                    break; default: ret += c;
                }
            }
            ret += '"';
            return ret;
        }
    }, obj);
}

}