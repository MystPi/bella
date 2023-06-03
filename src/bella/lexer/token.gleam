import nibble/lexer

pub type Tokens =
  List(lexer.Token(TokenT))

pub type TokenT {
  Ident(String)
  Number(Float)
  String(String)

  LParen
  RParen
  LBrace
  RBrace

  Plus
  Minus
  Star
  Slash

  Less
  Greater
  LessEq
  GreaterEq
  EqEq
  Neq

  Eq
  Colon
  Arrow
  Comma
  Dot
  RPipe
  Bang

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

  Comment(String)
}