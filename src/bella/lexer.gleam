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

type Matcher =
  #(String, fn(String) -> TokenType)

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
      #("true", token.True),
      #("false", token.False),
    ]
    |> map.from_list

  let matchers = [
    #("^[\\t ]+", i(token.WhiteSpace)),
    #("^\\n", i(token.Newline)),
    #("^;.*", i(token.Comment)),
    #("^\\(", i(token.LParen)),
    #("^\\)", i(token.RParen)),
    #("^\\{", i(token.LBrace)),
    #("^\\}", i(token.RBrace)),
    #("^\\|>", i(token.RPipe)),
    #("^->", i(token.Arrow)),
    #("^==", i(token.EqEq)),
    #("^!=", i(token.Neq)),
    #("^>=", i(token.GreaterEq)),
    #("^<=", i(token.LessEq)),
    #("^,", i(token.Comma)),
    #("^\\.", i(token.Dot)),
    #("^=", i(token.Eq)),
    #("^:", i(token.Colon)),
    #("^\\+", i(token.Plus)),
    #("^-", i(token.Minus)),
    #("^\\*", i(token.Star)),
    #("^/", i(token.Slash)),
    #("^>", i(token.Greater)),
    #("^<", i(token.Less)),
    #("^!", i(token.Bang)),
    #(
      "^'([^\\\\']|\\\\.)*'|^\"([^\\\\\"]|\\\\.)*\"",
      fn(x) { token.String(string.slice(x, 1, string.length(x) - 2)) },
    ),
    #(
      "^[a-zA-Z_]\\w*",
      fn(word) {
        case map.get(keywords, word) {
          Ok(keyword) -> keyword
          _ -> token.Ident(word)
        }
      },
    ),
    #("^\\d+(\\.\\d+)?", fn(x) { token.Number(to_float(x)) }),
  ]

  use tokens <- try(lex_str(str, matchers, [], 1, 0))

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
              token.Newline -> 0
              _ -> col + len
            },
          )
        None -> error.invalid_text(string.slice(str, 0, 10) <> "...")
      }
  }
}

fn match(str: String, matchers: List(Matcher)) -> Matched {
  use prev, matcher <- list.fold(matchers, None)
  option.or(prev, match_regex(matcher, str))
}

fn match_regex(matcher: Matcher, str: String) -> Matched {
  let #(regex_string, to_tok) = matcher
  let assert Ok(re) = regex.from_string(regex_string)

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

// UTILS .......................................................................

fn i(x) {
  fn(_) { x }
}

fn to_float(x: String) -> Float {
  case int.parse(x) {
    Ok(n) -> int.to_float(n)
    _ -> result.unwrap(float.parse(x), 0.0)
  }
}
