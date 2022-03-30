#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "operators.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/views.hpp"

#include <string_view>
#include <optional>
#include <tuple>
#include <vector>
#include <unordered_map>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void compiler_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

struct var_info
{
    std::size_t location;
    type_name   type;
    std::size_t type_size;
};

struct var_locations
{
    std::unordered_map<std::string, var_info> info;
    std::size_t next = 0;
};

struct function_def
{
    signature   sig;
    std::size_t ptr;
};

struct current_function
{
    var_locations vars;
    type_name     return_type;
};

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    anzu::program program;

    std::unordered_map<std::string, function_def> functions;

    var_locations globals;
    std::optional<current_function> current_func;

    type_store types;
};

auto current_vars(compiler& com) -> var_locations&
{
    return com.current_func ? com.current_func->vars : com.globals;
}

auto verify_real_type(const compiler& com, const token& tok, const type_name& t) -> void
{
    if (!com.types.is_registered_type(t)) {
        compiler_error(tok, "{} is not a recognised type", t);
    }
    if (!is_type_complete(t)) {
        compiler_error(tok, "generic function definitions currently disallowed ({})", t);
    }
}

template <typename T>
auto back(std::vector<T>& elements) -> T&
{
    return elements.back();
}

template <typename T>
auto append_op(compiler& com, T&& op) -> std::size_t
{
    com.program.emplace_back(std::forward<T>(op));
    return com.program.size() - 1;
}

// Registers the given name in the current scope
auto declare_variable_name(compiler& com, const std::string& name, const type_name& type) -> void
{
    auto& vars = current_vars(com);
    const auto type_size = com.types.block_size(type);
    const auto [iter, success] = vars.info.emplace(name, var_info{vars.next, type, type_size});
    if (success) { // If not successful, then the name already existed, so dont increase
        vars.next += type_size;
    }
}

auto find_variable(compiler& com, const std::string& name) -> void
{
    if (com.current_func && com.current_func->vars.info.contains(name)) {
        const auto& info = com.current_func->vars.info.at(name);
        com.program.emplace_back(anzu::op_push_local_addr{
            .offset=info.location, .size=info.type_size
        });
        return;
    }
    
    if (com.globals.info.contains(name)) {
        const auto& info = com.globals.info.at(name);
        com.program.emplace_back(anzu::op_push_global_addr{
            .position=info.location, .size=info.type_size
        });
        return;
    }

    anzu::print("could not find variable '{}'\n", name);
    std::exit(1);
}

auto save_variable(compiler& com, const std::string& name) -> void
{
    find_variable(com, name);
    com.program.emplace_back(anzu::op_save{});
}

auto load_variable(compiler& com, const std::string& name) -> void
{
    find_variable(com, name);
    com.program.emplace_back(anzu::op_load{});
}

auto signature_args_size(const compiler& com, const signature& sig) -> std::size_t
{
    auto args_size = std::size_t{0};
    for (const auto& arg : sig.args) {
        args_size += com.types.block_size(arg.type);
    }
    return args_size;
}

auto call_builtin(compiler& com, const std::string& function_name) -> void
{
    const auto& func = anzu::fetch_builtin(function_name);
    com.program.emplace_back(anzu::op_builtin_call{
        .name=function_name, .ptr=func.ptr, .args_size=signature_args_size(com, func.sig)
    });
}

auto find_function(const compiler& com, const std::string& function) -> const function_def*
{
    if (const auto it = com.functions.find(function); it != com.functions.end()) {
        return &it->second;
    }
    return nullptr;
}

auto offset_of_field(const compiler& com, const type_name& type, const std::string& field_name)
    -> std::tuple<std::size_t, type_name, std::size_t>
{
    auto offset     = std::size_t{0};
    auto size       = std::size_t{0};
    auto field_type = type_name{};
    for (const auto& field : com.types.get_fields(type)) {
        const auto field_size = com.types.block_size(field.type);
        if (field.name == field_name) {
            size = field_size;
            field_type = field.type;
            break;
        }
        offset += field_size;
    }
    if (size == 0) {
        print("type {} has no field '{}'\n", type, field_name);
        std::exit(1);
    }
    return std::tuple{offset, field_type, size};
}

auto compile_expr(compiler& com, const node_expr& expr) -> type_name;
auto compile_node(const node_stmt& root, compiler& com) -> void;
auto push_address_of(compiler& com, const node_expr& node) -> type_name;

auto push_address_of(compiler& com, const node_variable_expr& node) -> type_name
{
    if (com.current_func && com.current_func->vars.info.contains(node.name)) {
        const auto& info = com.current_func->vars.info.at(node.name);
        com.program.emplace_back(op_push_local_addr{
            .offset=info.location, .size=info.type_size
        });
        return info.type;
    }
    const auto& info = com.globals.info.at(node.name);
    com.program.emplace_back(op_push_global_addr{
        .position=info.location, .size=info.type_size
    });
    return info.type;
}

auto push_address_of(compiler& com, const node_field_expr& node) -> type_name
{
    const auto type = push_address_of(com, *node.expression);
    const auto [offset, field_type, size] = offset_of_field(com, type, node.field_name);
    com.program.emplace_back(op_modify_addr{
        .offset=offset, .new_size=size
    });
    return field_type;
}

auto push_address_of(compiler& com, const node_deref_expr& node) -> type_name
{
    const auto type = compile_expr(com, *node.expr); // Push the address
    const auto type_match = match(type, generic_ptr_type());
    if (!type_match) {
        print("tried to dereference an expression of type '{}'\n", type);
        std::exit(1);
    }
    return type_match->at(0);
}

auto push_address_of(compiler& com, const node_addrof_expr& node) -> type_name
{
    const auto type = push_address_of(com, *node.expr);
    return concrete_ptr_type(type);
}

auto push_address_of(compiler& com, const auto& node) -> type_name
{
    compiler_error(node.token, "cannot take address of a non-lvalue\n");
    return int_type();
}

auto push_address_of(compiler& com, const node_expr& node) -> type_name
{
    return std::visit([&](const auto& expr) { return push_address_of(com, expr); }, node);
}

// Both for and while loops have the form [<begin> <condition> <do> <body> <end>].
// This function links the do to jump to one past the end if false, makes breaks
// jump past the end, and makes continues jump back to the beginning.
void link_up_jumps(compiler& com, std::size_t begin, std::size_t jump, std::size_t end)
{
    // Jump past the end if false
    std::get<op_jump_if_false>(com.program[jump]).jump = end + 1;
        
    // Only set unset jumps, there may be other already set from nested loops
    for (std::size_t idx = jump + 1; idx != end; ++idx) {
        std::visit(overloaded{
            [&](op_break& op) {
                if (op.jump == 0) { op.jump = end + 1; }
            },
            [&](op_continue& op) {
                if (op.jump == 0) { op.jump = begin; }
            },
            [](auto&&) {}
        }, com.program[idx]);
    }
}

void verify_sig(const token& tok, const signature& sig, const std::vector<type_name>& args)
{
    if (sig.args.size() != args.size()) {
        compiler_error(tok, "function expected {} args, got {}", sig.args.size(), args.size());
    }

    for (const auto& [actual, expected] : zip(args, sig.args)) {
        if (actual != expected.type) {
            compiler_error(tok, "'{}' does not match '{}'", actual, expected.type);
        }
    }
}

auto make_constructor_sig(const compiler& com, const type_name& type) -> signature
{
    auto sig = signature{};
    for (const auto& field : com.types.get_fields(type)) {
        sig.args.emplace_back(field.name, field.type);
    }
    sig.return_type = type;
    return sig;
}

auto compile_expr(compiler& com, const node_expr& expr, const node_literal_expr& node) -> type_name
{
    com.program.emplace_back(anzu::op_load_literal{ .value=node.value.data });
    return node.value.type;
}

auto compile_expr(compiler& com, const node_expr& expr, const node_variable_expr& node) -> type_name
{
    const auto type = push_address_of(com, expr);
    com.program.emplace_back(op_load{});
    return type;
}

auto compile_expr(compiler& com, const node_expr& expr, const node_field_expr& node) -> type_name
{
    const auto type = push_address_of(com, expr);
    com.program.emplace_back(op_load{});
    return type;
}

// This is a copy of the logic from typecheck.cpp now, pretty bad, we should make it more
// generic and combine the logic.
auto compile_expr(compiler& com, const node_expr& expr, const node_binary_op_expr& node) -> type_name
{
    const auto lhs_type = compile_expr(com, *node.lhs);
    const auto rhs_type = compile_expr(com, *node.rhs);
    const auto op = node.token.text;

    const auto info = resolve_bin_op({.op = op, .lhs = lhs_type, .rhs = rhs_type});
    if (!info) {
        compiler_error(node.token, " could not evaluate '{} {} {}'", lhs_type, op, rhs_type);
    }

    com.program.emplace_back(op_builtin_mem_op{
        .name = std::format("{} {} {}", lhs_type, op, rhs_type),
        .ptr = info->operator_func
    });
    return info->result_type;
}

auto compile_expr(compiler& com, const node_expr& expr, const node_unary_op_expr& node) -> type_name
{
    const auto type = compile_expr(com, *node.expr);
    const auto op = node.token.text;
    const auto info = resolve_unary_op({.op = op, .type = type});
    if (!info) {
        compiler_error(node.token, " could not evaluate '{}{}'", op, type);
    }

    com.program.emplace_back(op_builtin_mem_op{
        .name = std::format("{}{}", op, type),
        .ptr = info->operator_func
    });
    return info->result_type;
} 

auto compile_expr(compiler& com, const node_expr& expr, const node_function_call_expr& node) -> type_name
{
    // Push the args to the stack
    std::vector<type_name> param_types;
    for (const auto& arg : node.args) {
        param_types.emplace_back(compile_expr(com, *arg));
    }

    // If this is the name of a simple type, then this is a constructor call, so
    // there is currently nothing to do since the arguments are already pushed to
    // the stack.
    if (const auto type = make_type(node.function_name); com.types.is_registered_type(type)) {
        const auto sig = make_constructor_sig(com, type);
        verify_sig(node.token, sig, param_types);
        return type;
    }

    // Otherwise, it may be a custom function.
    else if (const auto function_def = find_function(com, node.function_name)) {
        verify_sig(node.token, function_def->sig, param_types);
        com.program.emplace_back(anzu::op_function_call{
            .name=node.function_name,
            .ptr=function_def->ptr + 1, // Jump into the function
            .args_size=signature_args_size(com, function_def->sig),
            .return_size=com.types.block_size(function_def->sig.return_type)
        });
        return function_def->sig.return_type;
    }

    // Otherwise, it must be a builtin function.
    const auto& builtin = anzu::fetch_builtin(node.function_name);

    // TODO: Make this more generic, but we need to fill in the types before
    // calling here, so that we can pass in the correct block count
    auto sig = builtin.sig;
    if (node.function_name == "print" || node.function_name == "println") {
        sig.args[0].type = param_types[0];
    }

    com.program.emplace_back(anzu::op_builtin_call{
        .name=node.function_name,
        .ptr=builtin.ptr,
        .args_size=signature_args_size(com, sig)
    });
    return sig.return_type;
}

auto compile_expr(compiler& com, const node_expr& expr, const node_list_expr& node) -> type_name
{
    if (node.elements.empty()) {
        print("currently do not support empty list literals\n");
        std::exit(1);
    }
    auto element_view = node.elements | std::views::reverse;
    const auto inner_type = compile_expr(com, *element_view.front());
    for (const auto& element : element_view | std::views::drop(1)) {
        const auto element_type = compile_expr(com, *element);
        if (element_type != inner_type) {
            print("list has mismatching element types\n");
            std::exit(1);
        }
    }
    com.program.emplace_back(op_build_list{ .size = node.elements.size() });
    return concrete_list_type(inner_type);
}

auto compile_expr(compiler& com, const node_expr& expr, const node_addrof_expr& node) -> type_name
{
    return push_address_of(com, expr);
}

auto compile_expr(compiler& com, const node_expr& expr, const node_deref_expr& node) -> type_name
{
    const auto type = compile_expr(com, *node.expr);
    com.program.emplace_back(op_load{});

    const auto type_match = match(type, generic_ptr_type());
    if (!type_match) {
        print("tried to dereference an expression of type '{}'\n", type);
        std::exit(1);
    }
    return type_match->at(0);
}

void compile_node(const node_sequence_stmt& node, compiler& com)
{
    for (const auto& seq_node : node.sequence) {
        compile_node(*seq_node, com);
    }
}

void compile_node(const node_while_stmt& node, compiler& com)
{
    const auto begin_pos = append_op(com, op_loop_begin{});
    const auto condition_type = compile_expr(com, *node.condition);
    if (condition_type != bool_type()) {
        compiler_error(node.token, "while statement expected bool, got {}", condition_type);
    }
    const auto jump_pos = append_op(com, op_jump_if_false{});
    compile_node(*node.body, com);
    const auto end_pos = append_op(com, op_loop_end{ .jump=begin_pos });
    link_up_jumps(com, begin_pos, jump_pos, end_pos);
}

void compile_node(const node_if_stmt& node, compiler& com)
{
    const auto if_pos = append_op(com, op_if{});
    const auto condition_type = compile_expr(com, *node.condition);
    if (condition_type != bool_type()) {
        compiler_error(node.token, "if statement expected bool, got {}", condition_type);
    }
    const auto jump_pos = append_op(com, op_jump_if_false{});
    compile_node(*node.body, com);

    if (node.else_body) {
        const auto else_pos = append_op(com, op_else{});
        compile_node(*node.else_body, com);
        com.program.emplace_back(anzu::op_if_end{});
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = else_pos + 1; // Jump into the else block if false
        std::get<op_else>(com.program[else_pos]).jump = com.program.size(); // Jump past the end if false
    } else {
        com.program.emplace_back(anzu::op_if_end{});
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false
    }
}

void compile_node(const node_struct_stmt& node, compiler& com)
{
    if (com.types.is_registered_type(node.name)) {
        print("type '{}' is already defined\n", node.name);
        std::exit(1);
    }
    for (const auto& field : node.fields) {
        if (!com.types.is_registered_type(field.type)) {
            print(
                "unknown type '{}' of field {} for struct {}\n",
                field.type, field.name, node.name
            );
            std::exit(1);
        }
    }
    com.types.register_type(node.name, node.fields);
}

// TODO: This only works if the contained type has size 1, because lists are broken
void compile_node(const node_for_stmt& node, compiler& com)
{
    const auto container_name = std::string{"_Container"};
    const auto index_name = std::string{"_Index"};

    // Push the container to the stack
    const auto container_type = compile_expr(com, *node.container);
    const auto m = match(container_type, generic_list_type());
    if (!m) {
        print("error, {} must be a list type\n", container_type);
        std::exit(1);
    }
    const auto contained_type = m->at(0);

    declare_variable_name(com, container_name, container_type);
    save_variable(com, container_name); // Currently only lists are allowed in for stmts

    // Push the counter to the stack
    com.program.emplace_back(anzu::op_load_literal{
        .value=make_int(0).data
    });
    declare_variable_name(com, index_name, int_type());
    save_variable(com, index_name);

    declare_variable_name(com, node.var, contained_type);

    const auto begin_pos = append_op(com, op_loop_begin{});

    load_variable(com, index_name);
    load_variable(com, container_name);
    call_builtin(com, "list_size");

    const auto info = resolve_bin_op({
        .op = std::string{tk_ne}, .lhs = int_type(), .rhs = int_type()
    });
    com.program.emplace_back(op_builtin_mem_op{
        .name = std::format("{} {} {}", int_type(), std::string{tk_ne}, int_type()),
        .ptr = info->operator_func
    });
    
    const auto jump_pos = append_op(com, op_jump_if_false{}); // If size == index, jump to end

    load_variable(com, container_name);
    load_variable(com, index_name);
    call_builtin(com, "list_at");
    save_variable(com, node.var);

    compile_node(*node.body, com);

    // Increment the index
    load_variable(com, index_name);
    com.program.emplace_back(op_builtin_mem_op{
        .name = "increment",
        .ptr = +[](std::vector<block>& mem) {
            ++std::get<block_int>(back(mem));
        }
    });
    save_variable(com, index_name);

    const auto end_pos = append_op(com, op_loop_end{ .jump=begin_pos });

    link_up_jumps(com, begin_pos, jump_pos, end_pos);
}

void compile_node(const node_break_stmt&, compiler& com)
{
    com.program.emplace_back(anzu::op_break{});
}

void compile_node(const node_continue_stmt&, compiler& com)
{
    com.program.emplace_back(anzu::op_continue{});
}

void compile_node(const node_declaration_stmt& node, compiler& com)
{
    const auto type = compile_expr(com, *node.expr);
    if (current_vars(com).info.contains(node.name)) {
        compiler_error(node.token, "redeclaration of variable '{}'", node.name);
    }
    declare_variable_name(com, node.name, type);
    save_variable(com, node.name);
}

void compile_node(const node_assignment_stmt& node, compiler& com)
{
    const auto rhs_type = compile_expr(com, *node.expr);
    const auto lhs_type = push_address_of(com, *node.position);
    if (lhs_type != rhs_type) {
        print("cannot assign a {} to a {}\n", rhs_type, lhs_type);
        std::exit(1);
    }
    com.program.emplace_back(op_save{});
}

auto check_function_ends_with_return(const node_function_def_stmt& node) -> void
{
    // Functions returning null don't need a return statement.
    if (node.sig.return_type == null_type()) {
        return;
    }

    const auto bad_function = [&]() {
        compiler_error(node.token, "function '{}' does not end in a return statement\n", node.name);
    };

    const auto& body = *node.body;
    if (std::holds_alternative<node_return_stmt>(body)) {
        return;
    }

    if (std::holds_alternative<node_sequence_stmt>(body)) {
        const auto& seq = std::get<node_sequence_stmt>(body).sequence;
        if (seq.empty() || !std::holds_alternative<node_return_stmt>(*seq.back())) {
            bad_function();
        }
    }
    else {
        bad_function();
    }
}

void compile_node(const node_function_def_stmt& node, compiler& com)
{
    for (const auto& arg : node.sig.args) {
        verify_real_type(com, node.token, arg.type);
    }
    verify_real_type(com, node.token, node.sig.return_type);
    check_function_ends_with_return(node);

    const auto begin_pos = append_op(com, op_function{ .name=node.name });
    com.functions[node.name] = { .sig=node.sig ,.ptr=begin_pos };

    com.current_func.emplace(current_function{ .vars={}, .return_type=node.sig.return_type });
    for (const auto& arg : node.sig.args) {
        declare_variable_name(com, arg.name, arg.type);
    }
    compile_node(*node.body, com);
    com.current_func.reset();

    const auto end_pos = append_op(com, op_function_end{});

    std::get<anzu::op_function>(com.program[begin_pos]).jump = end_pos + 1;
}

void compile_node(const node_return_stmt& node, compiler& com)
{
    if (!com.current_func) {
        compiler_error(node.token, "return statements can only be within functions");
    }
    const auto return_type = compile_expr(com, *node.return_value);
    if (return_type != com.current_func->return_type) {
        compiler_error(
            node.token,
            "mismatched return type, expected {}, got {}",
            com.current_func->return_type, return_type
        );
    }
    com.program.emplace_back(anzu::op_return{});
}

void compile_node(const node_expression_stmt& node, compiler& com)
{
    const auto type = compile_expr(com, *node.expr);
    com.program.emplace_back(anzu::op_pop{ .size=com.types.block_size(type) });
}

auto compile_expr(compiler& com, const node_expr& expr) -> type_name
{
    return std::visit([&](const auto& node) { return compile_expr(com, expr, node); }, expr);
}

auto compile_node(const node_stmt& root, compiler& com) -> void
{
    std::visit([&](const auto& node) { compile_node(node, com); }, root);
}

}

auto compile(const node_stmt_ptr& root) -> anzu::program
{
    anzu::compiler com;
    compile_node(*root, com);
    return com.program;
}

}