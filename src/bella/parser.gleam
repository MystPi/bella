import gleam/list
import gleam/result.{try}
import bella/error
import bella/lexer/token.{Token, Tokens}

// expr := pipe | lambda | let_expr | if_expr

// lambda := Ident '->' expr
// let_expr := 'let' Ident '=' expr 'in' expr
// if_expr := 'if' '(' expr ')' expr 'else' expr

// pipe := logic_or ( '|>' logic_or ) *
// logic_or := logic_and ( 'or' logic_and )*
// logic_and := equality ( 'and' equality )*
// equality := comparison ( ( '==' | '!=' ) comparison )*
// comparison := term ( ( '>' | '>=' | '<' | '<=' ) term )*
// term := factor ( ( '+' | '-' ) factor )*
// factor := unary ( ( '/' | '*' ) unary )*
// unary := ( '-' | '!' ) unary | call
// call := primary ( '(' expr ( ',' expr )* ')' )*

// primary := Ident | Number | String | bool | block

// block := '{' expr+ '}'
// bool := True | False

pub type Expr {
  Var(String)
  String(String)
  Number(Float)
  Bool(Bool)
  Block(List(Expr))
  BinOp(operator: Token, left: Expr, right: Expr)
  Unary(operator: Token, value: Expr)
  Lambda(param: String, body: Expr)
  Let(name: String, value: Expr, body: Expr)
  Call(callee: Expr, arg: Expr)
  If(cond: Expr, true_branch: Expr, false_branch: Expr)
}

pub type Parsed =
  Result(#(Expr, Tokens), error.Error)

pub type Parser =
  fn(Tokens) -> Parsed

pub fn parse(tokens: Tokens) -> Result(List(Expr), error.Error) {
  use result <- try(parse_exprs(tokens, []))
  result
  |> list.reverse
  |> Ok
}

pub fn parse_exprs(
  tokens: Tokens,
  exprs: List(Expr),
) -> Result(List(Expr), error.Error) {
  case tokens {
    [token.Eof] -> Ok(exprs)
    _ -> {
      use #(expr, rest) <- try(parse_expr(tokens))
      parse_exprs(rest, [expr, ..exprs])
    }
  }
}

pub fn parse_expr(tokens: Tokens) -> Parsed {
  case tokens {
    [token.If, ..rest] -> parse_if(rest)
    [token.Let, ..rest] -> parse_let(rest)
    [token.Ident(n), token.Arrow, ..rest] -> {
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(Lambda(n, body), rest))
    }
    _ -> parse_pipe(tokens)
  }
}

pub fn expect(
  tok: Token,
  tokens: Tokens,
  msg: String,
  callback: fn(Tokens) -> Parsed,
) -> Parsed {
  case tokens {
    [head, ..tail] if head == tok -> callback(tail)
    _ -> error.expected(msg)
  }
}

pub fn parse_let(tokens: Tokens) -> Parsed {
  case tokens {
    [token.Ident(name), ..rest] -> {
      use rest <- expect(token.Eq, rest, "= after identifier")
      use #(value, rest) <- try(parse_expr(rest))
      use rest <- expect(token.In, rest, "`in` after initializer")
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(Let(name, value, body), rest))
    }
    _ -> error.expected("identifier after let")
  }
}

pub fn parse_if(tokens: Tokens) -> Parsed {
  use rest <- expect(token.LParen, tokens, "( before condition")
  use #(condition, rest) <- try(parse_expr(rest))
  use rest <- expect(token.RParen, rest, ") after condition")
  use #(true_branch, rest) <- try(parse_expr(rest))
  use rest <- expect(token.Else, rest, "`else` after true branch")
  use #(false_branch, rest) <- try(parse_expr(rest))
  Ok(#(If(condition, true_branch, false_branch), rest))
}

pub fn parse_pipe(tokens: Tokens) -> Parsed {
  parse_binop([token.RPipe], parse_logic_or, tokens)
}

pub fn parse_logic_or(tokens: Tokens) -> Parsed {
  parse_binop([token.Or], parse_logic_and, tokens)
}

pub fn parse_logic_and(tokens: Tokens) -> Parsed {
  parse_binop([token.And], parse_equality, tokens)
}

pub fn parse_equality(tokens: Tokens) -> Parsed {
  parse_binop([token.EqEq, token.Neq], parse_comparison, tokens)
}

pub fn parse_comparison(tokens: Tokens) -> Parsed {
  parse_binop(
    [token.Greater, token.GreaterEq, token.Less, token.LessEq],
    parse_term,
    tokens,
  )
}

pub fn parse_term(tokens: Tokens) -> Parsed {
  parse_binop([token.Plus, token.Minus], parse_factor, tokens)
}

pub fn parse_factor(tokens: Tokens) -> Parsed {
  parse_binop([token.Star, token.Slash], parse_unary, tokens)
}

pub fn parse_unary(tokens: Tokens) -> Parsed {
  case tokens {
    [token.Minus as op, ..rest] | [token.Bang as op, ..rest] -> {
      use #(expr, rest) <- try(parse_unary(rest))
      Ok(#(Unary(op, expr), rest))
    }
    _ -> parse_call(tokens)
  }
}

pub fn parse_call(tokens: Tokens) -> Parsed {
  use #(expr, rest) <- try(parse_primary(tokens))
  case rest {
    [token.LParen, ..rest] -> {
      use #(arg, rest) <- try(parse_expr(rest))
      finish_call(Call(expr, arg), rest)
    }
    _ -> Ok(#(expr, rest))
  }
}

pub fn finish_call(callee: Expr, tokens: Tokens) -> Parsed {
  case tokens {
    [token.Comma, ..rest] -> {
      use #(arg, rest) <- try(parse_expr(rest))
      finish_call(Call(callee, arg), rest)
    }
    [token.RParen, ..rest] -> Ok(#(callee, rest))
    _ -> error.expected(", or )")
  }
}

pub fn parse_binop(ops: Tokens, subrule: Parser, tokens: Tokens) -> Parsed {
  use #(left, rest) <- try(subrule(tokens))
  finish_binop(left, ops, subrule, rest)
}

pub fn finish_binop(
  left: Expr,
  ops: Tokens,
  subrule: Parser,
  tokens: Tokens,
) -> Parsed {
  let [op, ..rest] = tokens
  case list.contains(ops, op) {
    True -> {
      use #(right, rest) <- try(subrule(rest))
      finish_binop(BinOp(op, left, right), ops, subrule, rest)
    }
    False -> Ok(#(left, tokens))
  }
}

pub fn parse_primary(tokens: Tokens) -> Parsed {
  case tokens {
    [token.Number(x), ..rest] -> Ok(#(Number(x), rest))
    [token.String(x), ..rest] -> Ok(#(String(x), rest))
    [token.Ident(x), ..rest] -> Ok(#(Var(x), rest))
    [token.True, ..rest] -> Ok(#(Bool(True), rest))
    [token.False, ..rest] -> Ok(#(Bool(False), rest))
    [token.LBrace, ..rest] -> parse_block(rest, [])
    [tok, ..] -> error.unexpected("token: " <> token.token_to_string(tok))
  }
}

pub fn parse_block(tokens: Tokens, acc: List(Expr)) -> Parsed {
  case tokens {
    [token.RBrace, ..rest] -> {
      case acc {
        [] -> error.unexpected("end of block")
        _ -> Ok(#(Block(list.reverse(acc)), rest))
      }
    }
    _ -> {
      use #(expr, rest) <- try(parse_expr(tokens))
      parse_block(rest, [expr, ..acc])
    }
  }
}
