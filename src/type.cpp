#include "type.hpp"
#include "utility/print.hpp"

namespace anzu {

auto to_string(const type& type) -> std::string
{
    return std::visit([](const auto& t) { return to_string(t); }, type);
}

auto to_string(const type_simple& type) -> std::string
{
    return type.name;
}

auto to_string(const type_compound& type) -> std::string
{
    const auto subtypes = format_comma_separated(
        type.subtypes,
        [](const auto& t) { return to_string(t); }
    );
    return std::format("{}<{}>", type.name, subtypes);
}

auto to_string(const type_generic& type) -> std::string
{
    return std::format("[{}]", type.id);
}

auto hash(const type& type) -> std::size_t
{
    return std::visit([](const auto& t) { return hash(t); }, type);
}

auto hash(const type_simple& type) -> std::size_t
{
    return std::hash<std::string>{}(type.name);
}

auto hash(const type_compound& type) -> std::size_t
{
    auto hash_value = std::hash<std::string>{}(type.name);
    for (const auto& subtype : type.subtypes) {
        hash_value ^= hash(subtype);
    }
    return hash_value;
}

auto hash(const type_generic& type) -> std::size_t
{
    return std::hash<int>{}(type.id);
}
    
}