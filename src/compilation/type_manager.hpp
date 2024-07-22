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

class type_manager
{
    using type_hash = decltype([](const type_name& t) { return anzu::hash(t); });
    using type_fields = std::vector<type_field>;
    std::unordered_map<type_name, type_fields, type_hash> d_classes;

public:
    auto add(const type_name& name, const type_fields& fields) -> bool;
    auto contains(const type_name& t) const -> bool;

    auto size_of(const type_name& t) const -> std::size_t;
    auto fields_of(const type_name& t) const -> type_fields;
};

}