#include <string>
#include <filesystem>
#include <utility>
#include <format>

#include "object.hpp"

namespace anzu {

struct template_struct_name
{
    std::filesystem::path module;
    std::string           name;

    auto operator==(const template_struct_name&) const -> bool = default;
};

struct template_function_name
{
    std::filesystem::path module;
    type_struct           struct_name;
    std::string           name;

    auto to_string() const -> std::string;
    auto operator==(const template_function_name&) const -> bool = default;
};

struct function_name
{
    std::filesystem::path  module;
    type_struct            struct_name;
    std::string            name;
    std::vector<type_name> templates;

    auto as_template() const -> template_function_name;
    auto to_string() const -> std::string;
    auto operator==(const function_name&) const -> bool = default;
};

}

template <>
struct std::formatter<anzu::template_function_name> : std::formatter<std::string>
{
    auto format(const anzu::template_function_name& type, auto& ctx) const {
        return std::formatter<std::string>::format(type.to_string(), ctx);
    }
};

template <>
struct std::formatter<anzu::function_name> : std::formatter<std::string>
{
    auto format(const anzu::function_name& type, auto& ctx) const {
        return std::formatter<std::string>::format(type.to_string(), ctx);
    }
};

template<>
struct std::hash<anzu::template_struct_name>
{
    auto operator()(const anzu::template_struct_name& name) const -> std::size_t
    {
        return std::hash<std::string>{}(name.name) ^ std::hash<std::string>{}(name.module.string());
    }
};

template<>
struct std::hash<anzu::template_function_name>
{
    auto operator()(const anzu::template_function_name& name) const -> std::size_t
    {
        return std::hash<std::string>{}(name.name) ^ std::hash<std::string>{}(name.module.string())
                                                   ^ name.struct_name.hash();
    }
};

template<>
struct std::hash<anzu::function_name>
{
    // TODO: hash the template args
    auto operator()(const anzu::function_name& name) const -> std::size_t
    {
        return std::hash<std::string>{}(name.name) ^ std::hash<std::string>{}(name.module.string())
                                                   ^ name.struct_name.hash();
    }
};