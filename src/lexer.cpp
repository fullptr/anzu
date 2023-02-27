#include "lexer.hpp"
#include "object.hpp"
#include "vocabulary.hpp"
#include "utility/print.hpp"
#include "utility/peekstream.hpp"

#include <algorithm>
#include <ranges>
#include <fstream>
#include <sstream>
#include <optional>
#include <iterator>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] auto lexer_error(
    std::int64_t lineno, std::int64_t col, std::string_view msg, Args&&... args
)
    -> void
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", lineno, col, formatted_msg);
    std::exit(1);
}

}

struct lex_context
{
    std::string::const_iterator start;
    std::string::const_iterator curr;
    std::string::const_iterator end;

    std::size_t line = 1;
    std::size_t col = 0;
};

auto valid(const lex_context& ctx) -> bool
{
    return ctx.curr != ctx.end;
}

auto peek(const lex_context& ctx) -> char
{
    return *ctx.curr;
}

auto peek_next(const lex_context& ctx) -> char
{
    if (ctx.curr == ctx.end) return '\0';
    return *std::next(ctx.curr);
}

auto advance(lex_context& ctx) -> char
{
    ++ctx.col;
    return *(ctx.curr++);
}

auto identifier_type(const lex_context& ctx) -> lex_token_type {
    return lex_token_type::placeholder;
}

auto skip_whitespace(lex_context& ctx) -> void
{
    while (valid(ctx)) {
        const char c = peek(ctx);
        switch (c) {
            case ' ':
            case '\r':
            case '\t': {
                advance(ctx);
            } break;
            case '\n': {
                ++ctx.curr;
                ++ctx.line;
                ctx.col = 0;
            } break;
            case '#': {
                while (valid(ctx) && peek(ctx) != '\n') {
                    advance(ctx);
                }
            } break;
            default: {
                return;
            }
        }
    }
}

auto make_token(const lex_context& ctx, lex_token_type type) -> lex_token
{
    return lex_token{
        .text = {ctx.start, ctx.curr},
        .line = ctx.line,
        .col = ctx.col,
        .type = type
    };
}

auto make_identifier(lex_context& ctx) -> lex_token
{
    while (std::isalpha(peek(ctx)) || std::isdigit(peek(ctx))) advance(ctx);
    return make_token(ctx, identifier_type(ctx));
}

auto make_number(lex_context& ctx) -> lex_token
{
    while (std::isdigit(*ctx.curr)) advance(ctx);

    // look for the fractional part
    if (peek(ctx) == '.' && std::isdigit(peek_next(ctx))) {
        advance(ctx); // consume the .
        while (std::isdigit(peek(ctx))) advance(ctx);
    }

    return make_token(ctx, lex_token_type::number);
}

auto scan_token(lex_context& ctx) -> lex_token
{
    skip_whitespace(ctx);
    ctx.start = ctx.curr;
    
    const auto c = advance(ctx);
    if (std::isalpha(c)) return make_identifier(ctx);
    if (std::isdigit(c)) return make_number(ctx);

    if (ctx.curr == ctx.end) return make_token(ctx, lex_token_type::eof);

    return make_token(ctx, lex_token_type::placeholder);
}

auto lex(const std::filesystem::path& file) -> lex_result
{
    // Loop over the lines in the program, and then split each line into tokens.
    // If a '//' comment symbol is hit, the rest of the line is ignored.
    std::vector<anzu::token> tokens;
    std::ifstream ifs{file};
    if (!ifs) {
        lexer_error(0, 0, "Could not find module {}\n", file.string());
    }

    auto result = lex_result{};
    result.source_code = std::string{std::istreambuf_iterator<char>{ifs}, {}};

    auto line = std::size_t{1};
    auto col = std::size_t{0};
    auto curr = result.source_code.begin();
    const auto end = result.source_code.begin();

    auto ctx = lex_context{
        .start = result.source_code.begin(),
        .curr = result.source_code.begin(),
        .end = result.source_code.end()
    };
    while (ctx.curr != ctx.end) {
        result.tokens.push_back(scan_token(ctx));
    }
    return result;
}

auto print_tokens(const lex_result& res) -> void
{
    for (const auto& token : res.tokens) {
        const auto text = std::format("'{}'", token.text);
        anzu::print("{:<15} - {:<20} {:<5} {:<5}\n", to_string(token.type), text, token.line, token.col);
    }
}

auto to_string(lex_token_type tt) -> std::string_view
{
    switch (tt) {
        case lex_token_type::eof: return "eof";
        case lex_token_type::placeholder: return "placeholder";
        case lex_token_type::number: return "number";
        case lex_token_type::identifier: return "identifier";
        default: return "::unknown::";
    }
}

}