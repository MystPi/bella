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
