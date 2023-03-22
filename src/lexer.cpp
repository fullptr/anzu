#include "lexer.hpp"
#include "object.hpp"
#include "utility/print.hpp"

#include <fstream>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void lexer_error(
    std::int64_t lineno, std::int64_t col, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", lineno, col, formatted_msg);
    std::exit(1);
}

}

auto lexer::valid() const -> bool
{
    return d_curr != d_end;
}

auto lexer::peek() const -> char
{
    return *d_curr;
}

auto lexer::peek_next() const -> char
{
    if (!valid()) return '\0';
    return *std::next(d_curr);
}

auto lexer::advance() -> char
{
    ++d_col;
    return *(d_curr++);
}

auto lexer::match(std::string_view expected) -> bool
{
    auto original_curr = d_curr; // so we can roll back if we dont match
    for (char c : expected) {
        if (!valid() || peek() != c) {
            d_curr = original_curr;
            return false;
        }
        advance();
    }
    return true;
}

// TODO: We can make this more efficient, but it's fine for now
auto identifier_type(std::string_view token) -> token_type
{
    if (token == "assert")   return token_type::kw_assert;
    if (token == "bool")     return token_type::kw_bool;
    if (token == "break")    return token_type::kw_break;
    if (token == "char")     return token_type::kw_char;
    if (token == "continue") return token_type::kw_continue;
    if (token == "default")  return token_type::kw_default;
    if (token == "delete")   return token_type::kw_delete;
    if (token == "else")     return token_type::kw_else;
    if (token == "f64")      return token_type::kw_f64;
    if (token == "false")    return token_type::kw_false;
    if (token == "for")      return token_type::kw_for;
    if (token == "fn")       return token_type::kw_function;
    if (token == "i32")      return token_type::kw_i32;
    if (token == "i64")      return token_type::kw_i64;
    if (token == "if")       return token_type::kw_if;
    if (token == "import")   return token_type::kw_import;
    if (token == "in")       return token_type::kw_in;
    if (token == "loop")     return token_type::kw_loop;
    if (token == "new")      return token_type::kw_new;
    if (token == "null")     return token_type::kw_null;
    if (token == "return")   return token_type::kw_return;
    if (token == "sizeof")   return token_type::kw_sizeof;
    if (token == "struct")   return token_type::kw_struct;
    if (token == "true")     return token_type::kw_true;
    if (token == "typeof")   return token_type::kw_typeof;
    if (token == "u64")      return token_type::kw_u64;
    if (token == "while")    return token_type::kw_while;
    if (token == "(")        return token_type::left_paren;
    if (token == ")")        return token_type::right_paren;
    if (token == "{")        return token_type::left_brace;
    if (token == "}")        return token_type::right_brace;
    if (token == ";")        return token_type::semicolon;
    if (token == ",")        return token_type::comma;
    if (token == ".")        return token_type::dot;
    if (token == "-")        return token_type::minus;
    if (token == "+")        return token_type::plus;
    if (token == "/")        return token_type::slash;
    if (token == "*")        return token_type::star;
    if (token == "!=")       return token_type::bang_equal;
    if (token == "!")        return token_type::bang;
    if (token == "==")       return token_type::equal_equal;
    if (token == "=")        return token_type::equal;
    if (token == "<=")       return token_type::less_equal;
    if (token == "<")        return token_type::less;
    if (token == ">=")       return token_type::greater_equal;
    if (token == ">")        return token_type::greater;
    if (token == "&")        return token_type::ampersand;
    if (token == "&&")       return token_type::ampersand_ampersand;
    if (token == ":=")       return token_type::colon_equal;
    if (token == ":")        return token_type::colon;
    if (token == "[")        return token_type::left_bracket;
    if (token == "]")        return token_type::right_bracket;
    if (token == "%")        return token_type::percent;
    if (token == "||")       return token_type::bar_bar;
    if (token == "|")        return token_type::bar;
    if (token == "->")       return token_type::arrow;
    return token_type::identifier;
}

auto lexer::make_token(token_type type) const -> token
{
    const auto text = std::string_view{d_start, d_curr};

    // ctx.col is currently the end of the token, hence the offset to the front
    return token{ .text=text, .line=d_line, .col=(d_col - text.size()), .type=type };
}

auto lexer::make_identifier() -> token
{
    while (std::isalpha(peek()) || std::isdigit(peek()) || peek() == '_') advance();
    return make_token(identifier_type({d_start, d_curr}));
}

auto lexer::make_number() -> token
{
    using namespace std::string_view_literals;
    using tt = token_type;

    while (std::isdigit(peek())) advance();
    const auto is_float = match(".");
    while (std::isdigit(peek())) advance(); // won't do anything if not a float

    static constexpr auto suffixes = {
        std::pair{"u64"sv, tt::uint64},
        std::pair{"u"sv,   tt::uint64},
        std::pair{"i32"sv, tt::int32},
        std::pair{"i64"sv, tt::int64},
        std::pair{"f64"sv, tt::float64}
    };
    for (const auto& [suffix, type] : suffixes) {
        if (match(suffix)) {
            auto tok = make_token(type);
            tok.text.remove_suffix(suffix.size());
            return tok;
        }
    }

    return make_token(is_float ? tt::float64 : tt::int64);
}

auto lexer::make_literal(char delimiter, token_type tt) -> token
{
    while (valid() && peek() != delimiter) {
        if (peek() == '\n') {
            d_line++;
            d_col = 1;
        }
        advance();
    }

    if (!valid()) lexer_error(d_line, d_col, "Unterminated string");
    advance(); // closing quote

    auto tok = make_token(tt);
    tok.text.remove_prefix(1); // remove leading "
    tok.text.remove_suffix(1); // remove trailing "
    return tok;
}

auto lexer::make_string() -> token
{
    return make_literal('"', token_type::string);
}

auto lexer::make_char() -> token
{
    const auto tok = make_literal('\'', token_type::character);
    if (const auto size = tok.text.size(); size != 1) {
        lexer_error(d_line, d_col, "Char literal is not one character! Got '{}' ({})", tok.text, size);
    }
    return tok;
}

auto read_file(const std::filesystem::path& file) -> std::unique_ptr<std::string>
{
    std::ifstream ifs{file};
    if (!ifs) {
        lexer_error(0, 0, "Could not find module {}\n", file.string());
    }

    using iter = std::istreambuf_iterator<char>;
    return std::make_unique<std::string>(iter{ifs}, iter{});
}

lexer::lexer(std::string_view source_code)
    : d_start{source_code.begin()}
    , d_curr{source_code.begin()}
    , d_end{source_code.end()}
{
}

auto lexer::get_token() -> token
{
    const auto skip_whitespace = [&] {
        while (valid()) {
            const char c = peek();
            switch (c) {
                case ' ':
                case '\r':
                case '\t': {
                    advance();
                } break;
                case '\n': {
                    advance();
                    ++d_line;
                    d_col = 1;
                } break;
                case '#': {
                    while (valid() && peek() != '\n') {
                        advance();
                    }
                } break;
                default: {
                    return;
                }
            }
        }
    };

    skip_whitespace();
    if (!valid()) return make_token(token_type::eof);

    d_start = d_curr;
    
    const auto c = advance();
    if (std::isalpha(c)) return make_identifier();
    if (std::isdigit(c)) return make_number();

    switch (c) {
        case '(': return make_token(token_type::left_paren);
        case ')': return make_token(token_type::right_paren);
        case '{': return make_token(token_type::left_brace);
        case '}': return make_token(token_type::right_brace);
        case '[': return make_token(token_type::left_bracket);
        case ']': return make_token(token_type::right_bracket);
        case ';': return make_token(token_type::semicolon);
        case ',': return make_token(token_type::comma);
        case '.': return make_token(token_type::dot);
        case '-': return make_token(
            match(">") ? token_type::arrow : token_type::minus);
        case '+': return make_token(token_type::plus);
        case '/': return make_token(token_type::slash);
        case '*': return make_token(token_type::star);
        case '%': return make_token(token_type::percent);
        case '!': return make_token(
            match("=") ? token_type::bang_equal : token_type::bang);
        case '=': return make_token(
            match("=") ? token_type::equal_equal : token_type::equal);
        case '<': return make_token(
            match("=") ? token_type::less_equal : token_type::less);
        case '>': return make_token(
            match("=") ? token_type::greater_equal : token_type::greater);
        case ':': return make_token(
            match("=") ? token_type::colon_equal : token_type::colon);
        case '|': return make_token(
            match("|") ? token_type::bar_bar : token_type::bar);
        case '&': return make_token(
            match("&") ? token_type::ampersand_ampersand : token_type::ampersand);
        case '\'':
            return make_char();
        case '"':
            return make_string();
    }

    lexer_error(d_line, d_col, "Unknown token");
}

auto lex_print(std::string_view source_code) -> void
{
    auto ctx = lexer{source_code};
    for (auto token = ctx.get_token(); token.type != token_type::eof; token = ctx.get_token()) {
        const auto text = std::format("'{}'", token.text);
        anzu::print("{:<15} - {:<20} {:<5} {:<5}\n", to_string(token.type), text, token.line, token.col);
    }
}

tokenstream::tokenstream(std::string_view source_code)
    : d_ctx{lexer(source_code)}
    , d_curr{d_ctx.get_token()}
    , d_next{d_ctx.get_token()}
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

}