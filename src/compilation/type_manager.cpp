#include "type_manager.hpp"
#include "object.hpp"

namespace anzu {

auto type_manager::add(const type_name& name, const type_fields& fields) -> bool
{
    if (d_classes.contains(name)) {
        return false;
    }
    d_classes.emplace(name, fields);
    return true;
}

auto type_manager::contains(const type_name& type) const -> bool
{
    return std::visit(overloaded{
        [](type_fundamental)                  { return true; },
        [&](const type_struct&)               { return d_classes.contains(type); },
        [&](const type_array& t)              { return contains(*t.inner_type); },
        [&](const type_span& t)               { return contains(*t.inner_type); },
        [&](const type_ptr& t)                { return contains(*t.inner_type); },
        [&](const type_function_ptr&)         { return true; },
        [&](const type_bound_method&)         { return true; },
        [&](const type_bound_builtin_method&) { return true; },
        [&](const type_arena&)                { return true; },
        [&](const type_type& t)               { return contains(*t.type_val); }
    }, type);
}

auto type_manager::size_of(const type_name& type) const -> std::size_t
{
    return std::visit(overloaded{
        [](type_fundamental t) -> std::size_t {
            switch (t) {
                case type_fundamental::null_type:
                case type_fundamental::bool_type:
                case type_fundamental::char_type:
                    return 1;
                case type_fundamental::i32_type:
                    return 4;
                case type_fundamental::i64_type:
                case type_fundamental::u64_type:
                case type_fundamental::f64_type:
                case type_fundamental::nullptr_type:
                    return 8;
                default:
                    panic("unknown fundamental type");
                return 0;
            }
        },
        [&](const type_struct&) -> std::size_t {
            if (!d_classes.contains(type)) {
                panic("unknown type '{}'", type);
            }
            auto size = std::size_t{0};
            for (const auto& field : fields_of(type)) {
                size += size_of(field.type);
            }
            return std::max(std::size_t{1}, size); // empty structs take up one byte
        },
        [&](const type_array& t) {
            return size_of(*t.inner_type) * t.count;
        },
        [](const type_ptr&) {
            return sizeof(std::byte*);
        },
        [](const type_span&) {
            return sizeof(std::byte*) + sizeof(std::size_t);
        },
        [](const type_function_ptr&) {
            return sizeof(std::byte*);
        },
        [](const type_bound_method&) {
            return sizeof(std::byte*); // pointer to the object, first arg to the function
        },
        [](const type_bound_builtin_method&) {
            return sizeof(std::byte*); // pointer to the object, first arg to the function
        },
        [](const type_arena& arena) {
            return sizeof(std::byte*); // the runtime will store the arena separately
        },
        [&](const type_type&) {
            panic("tried to get the size of type: {}", type);
            return sizeof(void*);
        }
    }, type);
}

auto type_manager::fields_of(const type_name& t) const -> type_fields
{
    if (auto it = d_classes.find(t.remove_const()); it != d_classes.end()) {
        return it->second;
    }
    return {};
}

}