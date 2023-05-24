import gleam/float

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
  Throw
  If
  Else
  Or
  And
  True
  False
}

pub fn token_to_string(token: Token) -> String {
  case token {
    Eof -> "EOF"
    WhiteSpace -> "Whitespace"
    Comment -> "Comment"
    LParen -> "("
    RParen -> ")"
    LBrace -> "{"
    RBrace -> "}"
    Eq -> "="
    Arrow -> "->"
    Comma -> ","
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