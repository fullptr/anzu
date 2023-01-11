#include "token.hpp"
#include "utility/print.hpp"

#include <format>

namespace anzu {
namespace {

// Returns true if the value is either a keyword or a symbol, which are the only
// types expected to be consumed by the consume_*, peek, curr and next functions.
// Other token types are expected to be handled in other ways.
auto is_consumable_type(token_type type) -> bool
{
    return type == token_type::keyword || type == token_type::symbol;
}

}

auto token::error(std::string_view message) const -> void
{
    print("[ERROR] ({}:{}) {}\n", line, col, message);
    std::exit(1);
}

auto to_string(token_type type) -> std::string
{
    switch (type) {
        break; case token_type::keyword:   { return "keyword"; };
        break; case token_type::symbol:    { return "symbol"; };
        break; case token_type::name:      { return "name"; };
        break; case token_type::character: { return "character"; };
        break; case token_type::string:    { return "string"; };
        break; case token_type::i32:       { return "i32"; };
        break; case token_type::i64:       { return "i64"; };
        break; case token_type::u64:       { return "u64"; };
        break; case token_type::f64:       { return "f64"; };
        break; default:                    { return "UNKNOWN"; };
    }
}

auto print_tokens(const std::vector<anzu::token>& tokens) -> void
{
    for (const auto& token : tokens) {
        const auto text = std::format("'{}'", token.text);
        anzu::print("{:<10} - {:<20} {:<5} {:<5}\n", token.type, text, token.line, token.col);
    }
}

tokenstream::tokenstream(const std::vector<token>& tokens)
    : peekstream{tokens}
{}

auto tokenstream::consume_maybe(std::string_view text) -> bool
{
    if (valid() && curr().text == text && is_consumable_type(curr().type)) {
        consume();
        return true;
    }
    return false;
}

auto tokenstream::consume_only(std::string_view text) -> token
{
    if (!valid()) {
        anzu::print("[ERROR] (EOF) expected '{}'\n", text);
        std::exit(1);
    }
    if (curr().text != text || !is_consumable_type(curr().type)) {
        const auto [tok_text, line, col, type] = curr();
        anzu::print("[ERROR] ({}:{}) expected '{}', got '{}'\n", line, col, text, tok_text);
        std::exit(1);
    }
    return consume();
}

auto tokenstream::consume_i64() -> std::int64_t
{
    if (!valid()) {
        anzu::print("[ERROR] (EOF) expected an int\n");
        std::exit(1);
    }
    if (curr().type != token_type::i64) {
        const auto [tok_text, line, col, type] = curr();
        anzu::print("[ERROR] ({}:{}) expected an int, got '{}\n", line, col, tok_text);
        std::exit(1);
    }
    return std::stoll(consume().text);
}

auto tokenstream::consume_u64() -> std::uint64_t
{
    if (!valid()) {
        anzu::print("[ERROR] (EOF) expected a uint\n");
        std::exit(1);
    }
    if (curr().type != token_type::u64) {
        const auto [tok_text, line, col, type] = curr();
        anzu::print("[ERROR] ({}:{}) expected a uint, got '{}\n", line, col, tok_text);
        std::exit(1);
    }
    return std::stoull(consume().text);
}

auto tokenstream::peek(std::string_view text) -> bool
{
    return valid() && curr().text == text && is_consumable_type(curr().type);
}

auto tokenstream::peek_next(std::string_view text) -> bool
{
    return has_next() && next().text == text  && is_consumable_type(next().type);
}

}