#include <string>
#include <filesystem>
#include <utility>
#include <format>

#include "object.hpp"

namespace anzu {

struct function_name
{
    std::filesystem::path  module;
    type_struct            struct_name;
    std::string            name;
    std::vector<type_name> templates;

    auto as_template() const -> type_function_template;
    auto to_string() const -> std::string;
    auto to_hash() const -> std::size_t { return hash(module, struct_name, name, templates); }
    auto operator==(const function_name&) const -> bool = default;
};

}