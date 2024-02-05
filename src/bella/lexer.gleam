// Heavily inspired by https://github.com/DanielleMaywood/glexer
// Thank you very much!

import gleam/list
import gleam/result.{try}
import gleam/int
import gleam/float
import gleam/string
import bella/error
import bella/utils
import bella/lexer/token.{
  type Position, type Span, type Token, type TokenType, type Tokens, Position,
  Span,
}

// TYPES .......................................................................

pub type LexResult =
  Result(#(String, Position, Token), error.Error)

// LEXER .......................................................................

pub fn lex(str: String) -> Result(Tokens, error.Error) {
  use tokens <- try(lex_(str, Position(1, 1), []))

  tokens
  |> list.reverse
  |> Ok
}

fn lex_(
  input: String,
  pos: Position,
  tokens: Tokens,
) -> Result(Tokens, error.Error) {
  case next(input, pos) {
    Ok(#(_, _, #(token.Eof, _) as token)) -> Ok([token, ..tokens])
    Ok(#(rest, pos, token)) -> lex_(rest, pos, [token, ..tokens])
    Error(err) -> Error(err)
  }
}

fn next(input: String, pos: Position) -> LexResult {
  case input {
    // End of file
    "" -> token("", pos, token.Eof, 0)

    // Newline
    "\r\n" <> rest | "\n" <> rest -> next(rest, advance_line(pos))

    // Whitespace
    " " <> rest | "\t" <> rest -> next(rest, advance(pos, 1))

    // Comment
    ";" <> rest -> comment(rest, advance(pos, 1))

    // Grouping
    "(" <> rest -> token(rest, pos, token.LParen, 1)
    ")" <> rest -> token(rest, pos, token.RParen, 1)
    "{" <> rest -> token(rest, pos, token.LBrace, 1)
    "}" <> rest -> token(rest, pos, token.RBrace, 1)
    "[" <> rest -> token(rest, pos, token.LBracket, 1)
    "]" <> rest -> token(rest, pos, token.RBracket, 1)

    // Punctuation
    "|>" <> rest -> token(rest, pos, token.RPipe, 2)
    "|" <> rest -> token(rest, pos, token.Bar, 1)
    "->" <> rest -> token(rest, pos, token.Arrow, 2)
    "==" <> rest -> token(rest, pos, token.EqEq, 2)
    "!=" <> rest -> token(rest, pos, token.Neq, 2)
    ">=" <> rest -> token(rest, pos, token.GreaterEq, 2)
    "<=" <> rest -> token(rest, pos, token.LessEq, 2)
    "," <> rest -> token(rest, pos, token.Comma, 1)
    "." <> rest -> token(rest, pos, token.Dot, 1)
    "=" <> rest -> token(rest, pos, token.Eq, 1)
    ":" <> rest -> token(rest, pos, token.Colon, 1)
    "+" <> rest -> token(rest, pos, token.Plus, 1)
    "-" <> rest -> token(rest, pos, token.Minus, 1)
    "*" <> rest -> token(rest, pos, token.Star, 1)
    "/" <> rest -> token(rest, pos, token.Slash, 1)
    ">" <> rest -> token(rest, pos, token.Greater, 1)
    "<" <> rest -> token(rest, pos, token.Less, 1)
    "!" <> rest -> token(rest, pos, token.Bang, 1)
    "^" <> rest -> token(rest, pos, token.Caret, 1)
    "?" <> rest -> token(rest, pos, token.Question, 1)

    // String
    "\"" <> rest -> string(rest, pos, "", "\"")
    "'" <> rest -> string(rest, pos, "", "'")

    // Number
    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _ -> number(input, pos, "", IntMode)

    _ ->
      case string.pop_grapheme(input) {
        Ok(#(c, rest)) ->
          case is_alpha(c) {
            True -> ident(rest, pos, c)
            False ->
              error.syntax_error(
                "I don't understand what this means",
                pos_span(pos, 1),
              )
          }
        _ -> next(input, pos)
      }
  }
}

fn pos_span(pos: Position, span: Int) -> Span {
  Span(from: pos, to: Position(pos.line, pos.col + span))
}

fn advance_line(pos: Position) -> Position {
  Position(line: pos.line + 1, col: 1)
}

fn advance(pos: Position, offset: Int) -> Position {
  Position(line: pos.line, col: pos.col + offset)
}

fn token(
  input: String,
  pos: Position,
  token: TokenType,
  length: Int,
) -> LexResult {
  Ok(#(input, advance(pos, length), #(token, pos_span(pos, length))))
}

fn comment(input: String, pos: Position) -> LexResult {
  case input {
    "\r\n" <> rest | "\n" <> rest -> next(rest, advance_line(pos))

    source ->
      case string.pop_grapheme(source) {
        Ok(#(_, rest)) -> comment(rest, advance(pos, 1))
        _ -> next(source, pos)
      }
  }
}

fn string(
  input: String,
  start: Position,
  content: String,
  delim: String,
) -> LexResult {
  case input {
    "\\" <> rest ->
      case string.pop_grapheme(rest) {
        Ok(#(c, rest)) -> string(rest, start, content <> "\\" <> c, delim)
        _ -> string(rest, start, content <> "\\", delim)
      }

    "\r\n" <> _ | "\n" <> _ ->
      error.syntax_error(
        "Unterminated string",
        pos_span(start, string.length(content) + 2),
      )

    _ ->
      case string.pop_grapheme(input) {
        Ok(#(c, rest)) if c == delim ->
          token(
            rest,
            start,
            token.String(
              content
              |> utils.unescape,
            ),
            string.length(content) + 2,
          )

        Ok(#(c, rest)) -> string(rest, start, content <> c, delim)

        _ ->
          error.syntax_error(
            "Unterminated string",
            pos_span(start, string.length(content) + 2),
          )
      }
  }
}

type NumberMode {
  IntMode
  DotNoDecimal
  DotDecimal
}

fn decide(mode: NumberMode) -> NumberMode {
  case mode {
    IntMode -> IntMode
    DotNoDecimal -> DotDecimal
    DotDecimal -> DotDecimal
  }
}

fn number(
  input: String,
  start: Position,
  content: String,
  mode: NumberMode,
) -> LexResult {
  case input {
    "." <> rest if mode == IntMode ->
      number(rest, start, content <> ".", DotNoDecimal)

    "0" <> rest -> number(rest, start, content <> "0", decide(mode))
    "1" <> rest -> number(rest, start, content <> "1", decide(mode))
    "2" <> rest -> number(rest, start, content <> "2", decide(mode))
    "3" <> rest -> number(rest, start, content <> "3", decide(mode))
    "4" <> rest -> number(rest, start, content <> "4", decide(mode))
    "5" <> rest -> number(rest, start, content <> "5", decide(mode))
    "6" <> rest -> number(rest, start, content <> "6", decide(mode))
    "7" <> rest -> number(rest, start, content <> "7", decide(mode))
    "8" <> rest -> number(rest, start, content <> "8", decide(mode))
    "9" <> rest -> number(rest, start, content <> "9", decide(mode))

    _ ->
      case mode {
        IntMode | DotDecimal ->
          token(
            input,
            start,
            token.Number(to_float(content)),
            string.length(content),
          )

        DotNoDecimal ->
          error.syntax_error(
            "Expected decimal after decimal point",
            pos_span(start, string.length(content)),
          )
      }
  }
}

fn ident(input: String, start: Position, c: String) -> LexResult {
  let #(name, rest) = take_content(input, c, is_alphanum)

  let tok = case name {
    "let" -> token.Let
    "in" -> token.In
    "match" -> token.Match
    "is" -> token.Is
    "try" -> token.Try
    "throw" -> token.Throw
    "if" -> token.If
    "then" -> token.Then
    "else" -> token.Else
    "or" -> token.Or
    "and" -> token.And
    "import" -> token.Import
    "as" -> token.As
    "true" -> token.True
    "false" -> token.False
    _ -> token.Ident(name)
  }

  token(rest, start, tok, string.length(name))
}

// UTILS .......................................................................

fn take_content(
  input: String,
  content: String,
  predicate: fn(String) -> Bool,
) -> #(String, String) {
  case string.pop_grapheme(input) {
    Ok(#(grapheme, rest)) -> {
      case predicate(grapheme) {
        True -> take_content(rest, content <> grapheme, predicate)
        False -> #(content, input)
      }
    }
    _ -> #(content, "")
  }
}

fn to_float(x: String) -> Float {
  case int.parse(x) {
    Ok(n) -> int.to_float(n)
    _ -> result.unwrap(float.parse(x), 0.0)
  }
}

fn is_alphanum(c: String) {
  is_alpha(c) || is_num(c)
}

fn is_num(c: String) -> Bool {
  case c {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True

    _ -> False
  }
}

fn is_alpha(c: String) -> Bool {
  case c {
    "_"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True

    _ -> False
  }
}
