#include "token.hpp"
#include "utility/print.hpp"

#include <format>
#include <source_location>

namespace anzu {

void token::error(std::string_view message) const
{
    print("[ERROR] ({}:{}) {}\n", line, col, message);
    std::exit(1);
}

auto print_tokens(const std::vector<anzu::token>& tokens) -> void
{
    for (const auto& token : tokens) {
        const auto text = std::format("'{}'", token.text);
        anzu::print("{:<10} - {:<20} {:<5} {:<5}\n", token.type, text, token.line, token.col);
    }
}

tokenstream::tokenstream(const std::vector<token>& tokens)
    : d_begin(tokens.cbegin())
    , d_curr(tokens.cbegin())
    , d_end(tokens.cend())
{}

auto tokenstream::consume_maybe(token_type tt) -> bool
{
    if (valid() && curr().type == tt) {
        consume();
        return true;
    }
    return false;
}

auto tokenstream::consume_only(token_type tt, std::source_location loc) -> token
{
    if (!valid()) {
        anzu::print("[ERROR] (EOF) expected '{}'\n", tt);
        std::exit(1);
    }
    if (curr().type != tt) {
        curr().error("expected '{}', got '{}' [PARSER:{}]\n", tt, curr().type, loc.line());
    }
    return consume();
}

auto tokenstream::consume_i64() -> std::int64_t
{
    if (!valid()) {
        anzu::print("[ERROR] (EOF) expected an int\n");
        std::exit(1);
    }
    if (curr().type != token_type::int64) {
        curr().error("expected {}, got '{}'\n", token_type::int64, curr().type);
    }
    return std::stoll(std::string{consume().text}); // todo - use from_chars
}

auto tokenstream::consume_u64() -> std::uint64_t
{
    if (!valid()) {
        anzu::print("[ERROR] (EOF) expected a uint\n");
        std::exit(1);
    }
    if (curr().type != token_type::uint64) {
        curr().error("expected u64, got '{}'\n", token_type::uint64, curr().type);
    }
    return std::stoull(std::string{consume().text}); // todo - use from_chars
}

auto tokenstream::peek(token_type tt) -> bool
{
    return valid() && curr().type == tt;
}

auto tokenstream::peek_next(token_type tt) -> bool
{
    return has_next() && next().type == tt;
}

auto to_string(token_type tt) -> std::string_view
{
    switch (tt) {
        case token_type::ampersand_ampersand: return "&&";       
        case token_type::ampersand:           return "&";        
        case token_type::arrow:               return "->";       
        case token_type::bang_equal:          return "!=";       
        case token_type::bang:                return "!";        
        case token_type::bar_bar:             return "||";       
        case token_type::bar:                 return "|";        
        case token_type::colon_equal:         return ":=";       
        case token_type::colon:               return ":";        
        case token_type::comma:               return ",";    
        case token_type::character:           return "char";    
        case token_type::dot:                 return ".";        
        case token_type::equal_equal:         return "==";       
        case token_type::equal:               return "=";        
        case token_type::float64:             return "float64";
        case token_type::greater_equal:       return ">=";       
        case token_type::greater:             return ">";
        case token_type::identifier:          return "identifier";
        case token_type::int32:               return "int32";
        case token_type::int64:               return "int64";
        case token_type::kw_assert:           return "assert";
        case token_type::kw_bool:             return "bool";
        case token_type::kw_break:            return "break";
        case token_type::kw_char:             return "char";
        case token_type::kw_continue:         return "continue";
        case token_type::kw_default:          return "default";
        case token_type::kw_delete:           return "delete";
        case token_type::kw_else:             return "else";
        case token_type::kw_f64:              return "f64";
        case token_type::kw_false:            return "false";
        case token_type::kw_for:              return "for";
        case token_type::kw_function:         return "function";
        case token_type::kw_i32:              return "i32";
        case token_type::kw_i64:              return "i64";
        case token_type::kw_if:               return "if";
        case token_type::kw_import:           return "import";
        case token_type::kw_in:               return "in";
        case token_type::kw_loop:             return "loop";
        case token_type::kw_new:              return "new";
        case token_type::kw_null:             return "null";
        case token_type::kw_return:           return "return";
        case token_type::kw_sizeof:           return "sizeof";
        case token_type::kw_struct:           return "struct";
        case token_type::kw_true:             return "true";
        case token_type::kw_typeof:           return "typeof";
        case token_type::kw_u64:              return "u64";
        case token_type::kw_while:            return "while";
        case token_type::left_brace:          return "{";        
        case token_type::left_bracket:        return "[";        
        case token_type::left_paren:          return "(";        
        case token_type::less_equal:          return "<=";       
        case token_type::less:                return "<";        
        case token_type::minus:               return "-";        
        case token_type::percent:             return "%";        
        case token_type::plus:                return "+";        
        case token_type::right_brace:         return "}";        
        case token_type::right_bracket:       return "]";        
        case token_type::right_paren:         return ")";        
        case token_type::semicolon:           return ";";        
        case token_type::slash:               return "/";        
        case token_type::star:                return "*";       
        case token_type::string:              return "string-literal"; 
        case token_type::uint64:              return "uint64";
        default: return "::unknown::";
    }
}

}