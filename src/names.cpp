#include "names.hpp"

namespace anzu {

auto template_function_name::to_string() const -> std::string
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

auto function_name::as_template() const -> template_function_name
{
    return template_function_name{module, struct_name, name};
}

auto function_name::to_string() const -> std::string
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

}