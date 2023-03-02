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

auto tokenstream::consume_maybe(lex_token_type tt) -> bool
{
    if (valid() && curr().type == tt) {
        consume();
        return true;
    }
    return false;
}

auto tokenstream::consume_only(lex_token_type tt, std::source_location loc) -> token
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

auto tokenstream::peek(lex_token_type tt) -> bool
{
    return valid() && curr().type == tt;
}

auto tokenstream::peek_next(lex_token_type tt) -> bool
{
    return has_next() && next().type == tt;
}

auto to_string(lex_token_type tt) -> std::string_view
{
    switch (tt) {
        case lex_token_type::placeholder:         return "placeholder";
        case lex_token_type::int32:               return "int32";
        case lex_token_type::int64:               return "int64";
        case lex_token_type::uint64:              return "uint64";
        case lex_token_type::float64:             return "float64";
        case lex_token_type::identifier:          return "identifier";
        case lex_token_type::kw_assert:           return "assert";
        case lex_token_type::kw_bool:             return "bool";
        case lex_token_type::kw_break:            return "break";
        case lex_token_type::kw_char:             return "char";
        case lex_token_type::kw_continue:         return "continue";
        case lex_token_type::kw_default:          return "default";
        case lex_token_type::kw_delete:           return "delete";
        case lex_token_type::kw_else:             return "else";
        case lex_token_type::kw_f64:              return "f64";
        case lex_token_type::kw_false:            return "false";
        case lex_token_type::kw_for:              return "for";
        case lex_token_type::kw_function:         return "function";
        case lex_token_type::kw_i32:              return "i32";
        case lex_token_type::kw_i64:              return "i64";
        case lex_token_type::kw_if:               return "if";
        case lex_token_type::kw_import:           return "import";
        case lex_token_type::kw_in:               return "in";
        case lex_token_type::kw_loop:             return "loop";
        case lex_token_type::kw_new:              return "new";
        case lex_token_type::kw_null:             return "null";
        case lex_token_type::kw_return:           return "return";
        case lex_token_type::kw_sizeof:           return "sizeof";
        case lex_token_type::kw_struct:           return "struct";
        case lex_token_type::kw_true:             return "true";
        case lex_token_type::kw_typeof:           return "typeof";
        case lex_token_type::kw_u64:              return "u64";
        case lex_token_type::kw_while:            return "while";
        case lex_token_type::left_paren:          return "(";        
        case lex_token_type::right_paren:         return ")";        
        case lex_token_type::left_brace:          return "{";        
        case lex_token_type::right_brace:         return "}";        
        case lex_token_type::semicolon:           return ";";        
        case lex_token_type::comma:               return ",";        
        case lex_token_type::dot:                 return ".";        
        case lex_token_type::minus:               return "-";        
        case lex_token_type::plus:                return "+";        
        case lex_token_type::slash:               return "/";        
        case lex_token_type::star:                return "*";        
        case lex_token_type::bang_equal:          return "!=";       
        case lex_token_type::bang:                return "!";        
        case lex_token_type::equal_equal:         return "==";       
        case lex_token_type::equal:               return "=";        
        case lex_token_type::less_equal:          return "<=";       
        case lex_token_type::less:                return "<";        
        case lex_token_type::greater_equal:       return ">=";       
        case lex_token_type::greater:             return ">";
        case lex_token_type::ampersand:           return "&";        
        case lex_token_type::ampersand_ampersand: return "&&";       
        case lex_token_type::colon_equal:         return ":=";       
        case lex_token_type::colon:               return ":";        
        case lex_token_type::left_bracket:        return "[";        
        case lex_token_type::right_bracket:       return "]";        
        case lex_token_type::percent:             return "%";        
        case lex_token_type::bar_bar:             return "||";       
        case lex_token_type::bar:                 return "|";        
        case lex_token_type::arrow:               return "->";       
        default: return "::unknown::";
    }
}

}