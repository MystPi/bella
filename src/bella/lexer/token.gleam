import gleam/float

pub type Position {
  Position(from: Int, to: Int, line: Int)
}

pub type Token =
  #(TokenType, Position)

pub type Tokens =
  List(Token)

pub type TokenType {
  Eof
  WhiteSpace
  Newline
  Comment

  LParen
  RParen
  LBrace
  RBrace
  Eq
  Colon
  Arrow
  Comma
  Dot

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
  Throw
  If
  Else
  Or
  And
  True
  False
}

pub fn token_to_string(token: TokenType) -> String {
  case token {
    Eof -> "EOF"
    WhiteSpace -> "Whitespace"
    Newline -> "Newline"
    Comment -> "Comment"
    LParen -> "("
    RParen -> ")"
    LBrace -> "{"
    RBrace -> "}"
    Eq -> "="
    Colon -> ":"
    Arrow -> "->"
    Comma -> ","
    Dot -> "."
    Plus -> "+"
    Minus -> "-"
    Star -> "*"
    Slash -> "/"
    EqEq -> "=="
    Neq -> "!="
    GreaterEq -> ">="
    LessEq -> "<="
    RPipe -> "|>"
    Greater -> ">"
    Less -> "<"
    Bang -> "!"
    Ident(x) -> x
    String(x) -> "'" <> x <> "'"
    Number(x) -> float.to_string(x)
    Let -> "let"
    In -> "in"
    Try -> "try"
    Throw -> "throw"
    If -> "if"
    Else -> "else"
    Or -> "or"
    And -> "and"
    True -> "true"
    False -> "false"
  }
}
