#pragma once
#include "op_codes.hpp"
#include "object.hpp"

#include <memory>
#include <vector>

namespace anzu {

// I normally avoid inheritance trees, however dealing with variants here was a bit
// cumbersome and required wrapping the variant in a struct to allow recusrion (due
// to the variant needing to be defined after the nodes that contained them). Given
// that this will be a shall tree of types (depth of 1) I feel it's fine to use here.
struct node
{
    virtual ~node() {}

    virtual void evaluate(std::vector<anzu::op>& program) = 0;
    virtual void print(int indent = 0) = 0;
};

struct node_bin_op : public node
{
    std::string           op;
    std::unique_ptr<node> lhs;
    std::unique_ptr<node> rhs;

    void evaluate(std::vector<anzu::op>& program) override;
    void print(int indent = 0) override;
};

struct node_literal : public node
{
    anzu::object value;

    node_literal(const anzu::object& v) : value(v) {}

    void evaluate(std::vector<anzu::op>& program) override;
    void print(int indent = 0) override;
};

}