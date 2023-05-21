import gleam/list
import gleam/string
import gleam/regex
import gleam/result
import gleam/map
import gleam/int
import gleam/float
import gleam/option.{None, Option, Some}
import error

pub type Tokens =
  List(Token)

pub type Token {
  Eof
  WhiteSpace
  Comment

  LParen
  RParen
  LBrace
  RBrace
  Eq
  Arrow
  Comma

  Plus
  Minus
  Star
  Slash
  EqEq
  Neq
  GreaterEq
  LessEq
  RPipe
  Greater
  Less
  Bang

  Ident(String)
  String(String)
  Number(Float)

  Let
  In
  Try
  If
  Else
  Or
  And
  True
  False
}

type Matcher =
  #(String, fn(String) -> Token)

type Matched =
  Option(#(String, Token))

type LexResult =
  Result(Tokens, error.Error)

pub fn lex(str: String) -> LexResult {
  let keywords =
    [
      #("let", Let),
      #("in", In),
      #("try", Try),
      #("if", If),
      #("else", Else),
      #("or", Or),
      #("and", And),
      #("true", True),
      #("false", False),
    ]
    |> map.from_list

  let matchers = [
    #("^\\s+", i(WhiteSpace)),
    #("^;.*", i(Comment)),
    #("^\\(", i(LParen)),
    #("^\\)", i(RParen)),
    #("^\\{", i(LBrace)),
    #("^\\}", i(RBrace)),
    #("^\\|>", i(RPipe)),
    #("^->", i(Arrow)),
    #("^==", i(EqEq)),
    #("^!=", i(Neq)),
    #("^>=", i(GreaterEq)),
    #("^<=", i(LessEq)),
    #("^,", i(Comma)),
    #("^=", i(Eq)),
    #("^\\+", i(Plus)),
    #("^-", i(Minus)),
    #("^\\*", i(Star)),
    #("^/", i(Slash)),
    #("^>", i(Greater)),
    #("^<", i(Less)),
    #("^!", i(Bang)),
    #(
      "^'([^\\\\']|\\\\.)*'",
      fn(x) { String(string.slice(x, 1, string.length(x) - 2)) },
    ),
    #(
      "^[a-zA-Z_]\\w*",
      fn(word) {
        case map.get(keywords, word) {
          Ok(keyword) -> keyword
          _ -> Ident(word)
        }
      },
    ),
    #("^\\d+(\\.\\d+)?", fn(x) { Number(to_float(x)) }),
  ]

  let ignored = [WhiteSpace, Comment]

  use tokens <- result.then(lex_str(str, matchers, []))

  tokens
  |> list.filter(fn(x) { !list.contains(ignored, x) })
  |> list.reverse
  |> Ok
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

fn lex_str(str: String, matchers: List(Matcher), tokens: Tokens) -> LexResult {
  case str {
    "" -> Ok([Eof, ..tokens])
    _ ->
      case match(str, matchers) {
        Some(#(rest, tok)) -> lex_str(rest, matchers, [tok, ..tokens])
        None -> Error(error.InvalidText(string.slice(str, 0, 5) <> "..."))
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
      Some(#(string.drop_left(str, string.length(content)), to_tok(content)))
    [] -> None
  }
}
