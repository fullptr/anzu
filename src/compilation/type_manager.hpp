#pragma once
#include "object.hpp"

#include <unordered_map>
#include <stack>

namespace anzu {

struct type_field
{
    std::string name;
    type_name   type;
    auto operator==(const type_field&) const -> bool = default;
};

class type_manager
{
    using type_fields = std::vector<type_field>;
    std::unordered_map<type_name, type_fields, type_hash> d_classes;

    // When compiling a function template, the template names need to be
    // available, so we push them here. This is a stack since we can pause
    // compiling one function to start compiling another
    std::stack<template_map> d_template_args;

public:
    auto add(const type_name& name, const type_fields& fields) -> bool;
    auto contains(const type_name& t) const -> bool;

    // If the string doesn't match an existing type, a new type_struct is returned,
    // but not added to the store (since you'll add it with fields). Otherwise, it will
    // return the existing type or resolve it to a templated type.
    auto make_type(const std::string& name) -> type_name;

    auto size_of(const type_name& t) const -> std::size_t;
    auto fields_of(const type_name& t) const -> type_fields;

    auto push_template_types(const template_map& map) -> void
    {
        d_template_args.push(map);
    }

    auto pop_template_types() -> void
    {
        d_template_args.pop();
    }

    auto resolve_template(const type_name& type) const -> type_name;
};

}