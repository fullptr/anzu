#include <string>
#include <filesystem>
#include <utility>
#include <format>

#include "object.hpp"

namespace anzu {

struct template_struct_name
{
    std::string name;
    std::filesystem::path module;
    auto operator==(const template_struct_name&) const -> bool = default;
};

struct template_function_name
{
    std::filesystem::path module;
    type_struct           struct_name;
    std::string           name;
    auto operator==(const template_function_name&) const -> bool = default;
    auto to_string() const -> std::string
    {
        auto ret = std::format("<{}>", module.string());
        if (struct_name != type_struct{""}) {
            ret += std::format(".{}", struct_name.name);
            if (!struct_name.templates.empty()) {
                ret += std::format("!({})", format_comma_separated(struct_name.templates));
            }
        }
        ret += std::format(".{}", name);
        return ret;
    }
};

struct function_name
{
    std::filesystem::path  module;
    type_struct            struct_name;
    std::string            name;
    std::vector<type_name> templates;
    auto operator==(const function_name&) const -> bool = default;
    
    auto as_template() const -> template_function_name
    {
        return template_function_name{module, struct_name, name};
    }

    auto to_string() const -> std::string
    {
        auto ret = std::format("<{}>", module.string());
        if (struct_name != type_struct{""}) {
            ret += std::format(".{}", struct_name.name);
            if (!struct_name.templates.empty()) {
                ret += std::format("!({})", format_comma_separated(struct_name.templates));
            }
        }
        ret += std::format(".{}", name);
        if (!templates.empty()) {
            const auto template_args_string = format_comma_separated(templates);
            ret += std::format("!({})", template_args_string);
        }

        return ret;
    }
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
                                                   ^ anzu::hash(name.struct_name);
    }
};

template<>
struct std::hash<anzu::function_name>
{
    // TODO: hash the template args
    auto operator()(const anzu::function_name& name) const -> std::size_t
    {
        return std::hash<std::string>{}(name.name) ^ std::hash<std::string>{}(name.module.string())
                                                   ^ anzu::hash(name.struct_name);
    }
};