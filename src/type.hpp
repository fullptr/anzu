#pragma once
#include <string>
#include <variant>
#include <vector>

namespace anzu {

struct type;

struct type_simple
{
    std::string name;
};

struct type_compound
{
    std::string       name;
    std::vector<type> subtypes;
};

struct type_generic
{
    int id;
};

struct type : std::variant<type_simple, type_compound, type_generic> {};


    
}