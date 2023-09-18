pub type Position {
  Position(line: Int, col: Int)
}

pub type Span {
  Span(from: Position, to: Position)
}

pub type Token =
  #(TokenType, Span)

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
  LBracket
  RBracket
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
  Import
  As
  True
  False
}
