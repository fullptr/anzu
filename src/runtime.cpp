#include "runtime.hpp"
#include "print.hpp"

#include <utility>

namespace anzu {

auto memory::insert(const std::string& name, const anzu::object& value) -> anzu::object&
{
    auto [iter, success] = d_values.insert_or_assign(name, value);
    return iter->second;
}

auto memory::get(const std::string& name) -> anzu::object&
{
    if (!d_values.contains(name)) {
        anzu::print("Error: Unknown value '{}'", name);
        std::exit(1);
    }
    return d_values.at(name);
}

auto memory::print() const -> void
{
    anzu::print("Values:\n");
    for (const auto& [key, val] : d_values) {
        anzu::print(" - {} -> {}\n", key, val);
    }
}

auto runtime_context::push_frame() -> frame&
{
    d_frames.push_back({});
    return d_frames.back();
}

auto runtime_context::pop_frame() -> void
{
    d_frames.pop_back();
}

auto runtime_context::peek_frame(std::size_t index) -> frame&
{
    return d_frames[d_frames.size() - index - 1];
}

auto runtime_context::push_value(const object& val) -> object&
{
    d_values.push_back(val);
    return d_values.back();
}

auto runtime_context::pop_value() -> object
{
    auto ret = d_values.back();
    d_values.pop_back();
    return ret;
}

auto runtime_context::peek_value(std::size_t index) -> object&
{
    return d_values[d_values.size() - index - 1];
}

auto runtime_context::size() const -> std::size_t
{
    return d_values.size();
}

auto run_program(const std::vector<anzu::op>& program) -> void
{
    runtime_context ctx;
    ctx.push_frame();

    while (ctx.peek_frame().ptr < std::ssize(program)) {
        program[ctx.peek_frame().ptr].apply(ctx);
    }
}

auto run_program_debug(const std::vector<anzu::op>& program) -> void
{
    anzu::runtime_context ctx;
    ctx.push_frame();

    while (ctx.peek_frame().ptr < std::ssize(program)) {
        const auto& op = program[ctx.peek_frame().ptr];
        anzu::print("{:>4} - {}\n", ctx.peek_frame().ptr, op);
        op.apply(ctx);
    }
}

}