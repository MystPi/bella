import gleam/list
import gleam/string
import gleam/regex
import gleam/result.{try}
import gleam/map
import gleam/int
import gleam/float
import gleam/option.{None, Option, Some}
import bella/error
import bella/lexer/token.{TokenType, Tokens}

// TYPES .......................................................................

type Matcher {
  StringMatcher(String, TokenType)
  RegexMatcher(regex.Regex, fn(String) -> TokenType)
}

type Matched =
  Option(#(String, TokenType, Int))

type LexResult =
  Result(Tokens, error.Error)

// LEXER .......................................................................

pub fn lex(str: String) -> LexResult {
  let keywords =
    [
      #("let", token.Let),
      #("in", token.In),
      #("try", token.Try),
      #("throw", token.Throw),
      #("if", token.If),
      #("else", token.Else),
      #("or", token.Or),
      #("and", token.And),
      #("import", token.Import),
      #("as", token.As),
      #("true", token.True),
      #("false", token.False),
    ]
    |> map.from_list

  let matchers = [
    r("^[\\t ]+", i(token.WhiteSpace)),
    r("^\\n", i(token.Newline)),
    r("^;.*", i(token.Comment)),
    s("(", token.LParen),
    s(")", token.RParen),
    s("{", token.LBrace),
    s("}", token.RBrace),
    s("[", token.LBracket),
    s("]", token.RBracket),
    s("|>", token.RPipe),
    s("->", token.Arrow),
    s("==", token.EqEq),
    s("!=", token.Neq),
    s(">=", token.GreaterEq),
    s("<=", token.LessEq),
    s(",", token.Comma),
    s(".", token.Dot),
    s("=", token.Eq),
    s(":", token.Colon),
    s("+", token.Plus),
    s("-", token.Minus),
    s("*", token.Star),
    s("/", token.Slash),
    s(">", token.Greater),
    s("<", token.Less),
    s("!", token.Bang),
    r(
      "^'([^\\\\']|\\\\.)*'|^\"([^\\\\\"]|\\\\.)*\"",
      fn(x) { token.String(string.slice(x, 1, string.length(x) - 2)) },
    ),
    r(
      "^[a-zA-Z_]\\w*",
      fn(word) {
        case map.get(keywords, word) {
          Ok(keyword) -> keyword
          _ -> token.Ident(word)
        }
      },
    ),
    r("^\\d+(\\.\\d+)?", fn(x) { token.Number(to_float(x)) }),
  ]

  use tokens <- try(lex_str(str, matchers, [], 1, 1))

  tokens
  |> list.reverse
  |> Ok
}

fn lex_str(
  str: String,
  matchers: List(Matcher),
  tokens: Tokens,
  line: Int,
  col: Int,
) -> LexResult {
  case str {
    "" ->
      Ok([#(token.Eof, token.Position(#(line, col), #(line, col))), ..tokens])
    _ ->
      case match(str, matchers) {
        Some(#(rest, tok_type, len)) ->
          lex_str(
            rest,
            matchers,
            case tok_type {
              token.WhiteSpace | token.Newline | token.Comment -> tokens
              _ -> [
                #(tok_type, token.Position(#(line, col), #(line, col + len))),
                ..tokens
              ]
            },
            case tok_type {
              token.Newline -> line + 1
              _ -> line
            },
            case tok_type {
              token.Newline -> 1
              _ -> col + len
            },
          )
        None ->
          error.syntax_error(
            "I don't understand what this means",
            token.Position(#(line, col), #(line, col + 1)),
          )
      }
  }
}

fn match(str: String, matchers: List(Matcher)) -> Matched {
  use prev, matcher <- list.fold(matchers, None)
  option.or(prev, match_matcher(matcher, str))
}

fn match_matcher(matcher: Matcher, str: String) -> Matched {
  case matcher {
    StringMatcher(tok_str, tok_type) ->
      case string.starts_with(str, tok_str) {
        True ->
          Some(#(
            string.drop_left(str, string.length(tok_str)),
            tok_type,
            string.length(tok_str),
          ))
        _ -> None
      }
    RegexMatcher(re, to_tok) ->
      case regex.scan(re, str) {
        [regex.Match(content, _), ..] ->
          Some(#(
            string.drop_left(str, string.length(content)),
            to_tok(content),
            string.length(content),
          ))
        [] -> None
      }
  }
}

// UTILS .......................................................................

fn r(regex_string: String, to_tok: fn(String) -> TokenType) -> Matcher {
  let assert Ok(re) = regex.from_string(regex_string)
  RegexMatcher(re, to_tok)
}

fn s(s: String, tok_type: TokenType) -> Matcher {
  StringMatcher(s, tok_type)
}

fn i(x) {
  fn(_) { x }
}

fn to_float(x: String) -> Float {
  case int.parse(x) {
    Ok(n) -> int.to_float(n)
    _ -> result.unwrap(float.parse(x), 0.0)
  }
}
