pub type Position {
  Position(from: #(Int, Int), to: #(Int, Int))
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
