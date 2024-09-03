#pragma once
#include "object.hpp"

#include <unordered_map>

namespace anzu {

struct type_field
{
    std::string name;
    type_name   type;
    auto operator==(const type_field&) const -> bool = default;
};

using type_fields = std::vector<type_field>;

struct type_info
{
    type_fields fields;
    template_map templates;
};

class type_manager
{
    std::unordered_map<type_name, type_info> d_classes;

public:
    auto add(const type_name& name, const type_fields& fields, const template_map& templates = {}) -> bool;
    auto add_type(const type_name& name, const template_map& templates) -> bool;
    auto add_field(const type_name& name, const type_field& field) -> bool;
    auto contains(const type_name& t) const -> bool;

    auto size_of(const type_name& t) const -> std::size_t;
    auto fields_of(const type_name& t) const -> type_fields;
    auto templates_of(const type_name& t) const -> template_map;
};

}