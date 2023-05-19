import gleam/list
import gleam/string
import gleam/result.{then}
import data/ast
import data/error
import data/token.{Token, Tokens}

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

pub type Parsed =
  Result(#(ast.Expr, Tokens), error.Error)

pub type Parser =
  fn(Tokens) -> Parsed

pub fn parse(tokens: Tokens) -> Result(List(ast.Expr), error.Error) {
  use result <- then(parse_exprs(tokens, []))
  result
  |> list.reverse
  |> Ok
}

pub fn parse_exprs(
  tokens: Tokens,
  exprs: List(ast.Expr),
) -> Result(List(ast.Expr), error.Error) {
  case tokens {
    [token.Eof] -> Ok(exprs)
    _ -> {
      use #(expr, rest) <- then(parse_expr(tokens))
      parse_exprs(rest, [expr, ..exprs])
    }
  }
}

pub fn parse_expr(tokens: Tokens) -> Parsed {
  case tokens {
    [token.If, ..rest] -> parse_if(rest)
    [token.Let, ..rest] -> parse_let(rest)
    [token.Ident(n), token.Arrow, ..rest] -> {
      use #(body, rest) <- then(parse_expr(rest))
      Ok(#(ast.Lambda(n, body), rest))
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
      use rest <- expect(token.Eq, rest, "Expected = after identifier")
      use #(value, rest) <- then(parse_expr(rest))
      use rest <- expect(token.In, rest, "Expected `in` after initializer")
      use #(body, rest) <- then(parse_expr(rest))
      Ok(#(ast.Let(name, value, body), rest))
    }
    _ -> error.expected("Expected identifier after let")
  }
}

pub fn parse_if(tokens: Tokens) -> Parsed {
  use rest <- expect(token.LParen, tokens, "Expected ( before condition")
  use #(condition, rest) <- then(parse_expr(rest))
  use rest <- expect(token.RParen, rest, "Expected ) after condition")
  use #(true_branch, rest) <- then(parse_expr(rest))
  use rest <- expect(token.Else, rest, "Expected `else` after true branch")
  use #(false_branch, rest) <- then(parse_expr(rest))
  Ok(#(ast.If(condition, true_branch, false_branch), rest))
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
  parse_binop([token.Greater, token.GreaterEq, token.Less, token.LessEq], parse_term, tokens)
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
      use #(expr, rest) <- then(parse_unary(rest))
      Ok(#(ast.Unary(op, expr), rest))
    }
    _ -> parse_call(tokens)
  }
}

pub fn parse_call(tokens: Tokens) -> Parsed {
  use #(expr, rest) <- then(parse_primary(tokens))
  case rest {
    [token.LParen, ..rest] -> {
      use #(arg, rest) <- then(parse_expr(rest))
      finish_call(ast.Call(expr, arg), rest)
    }
    _ -> Ok(#(expr, rest))
  }
}

// pub fn finish_call(callee: ast.Expr, tokens: Tokens) -> Parsed {
//   use #(arg, rest) <- then(parse_expr(tokens))
//   use rest <- expect(token.RParen, rest, "Expect ) after call")
//   case rest {
//     [token.LParen, ..rest] -> finish_call(ast.Call(callee, arg), rest)
//     _ -> Ok(#(ast.Call(callee, arg), rest))
//   }
// }

pub fn finish_call(callee: ast.Expr, tokens: Tokens) -> Parsed {
  case tokens {
    [token.Comma, ..rest] -> {
      use #(arg, rest) <- then(parse_expr(rest))
      finish_call(ast.Call(callee, arg), rest)
    }
    [token.RParen, ..rest] -> Ok(#(callee, rest))
    _ -> error.expected("Expected , or )")
  }
}

pub fn parse_binop(ops: Tokens, subrule: Parser, tokens: Tokens) -> Parsed {
  use #(left, rest) <- then(subrule(tokens))
  finish_binop(left, ops, subrule, rest)
}

pub fn finish_binop(
  left: ast.Expr,
  ops: Tokens,
  subrule: Parser,
  tokens: Tokens,
) -> Parsed {
  let [op, ..rest] = tokens
  case list.contains(ops, op) {
    True -> {
      use #(right, rest) <- then(subrule(rest))
      finish_binop(ast.BinOp(op, left, right), ops, subrule, rest)
    }
    False -> Ok(#(left, tokens))
  }
}

pub fn parse_primary(tokens: Tokens) -> Parsed {
  case tokens {
    [token.Number(x), ..rest] -> Ok(#(ast.Number(x), rest))
    [token.String(x), ..rest] -> Ok(#(ast.String(x), rest))
    [token.Ident(x), ..rest] -> Ok(#(ast.Var(x), rest))
    [token.True, ..rest] -> Ok(#(ast.Bool(True), rest))
    [token.False, ..rest] -> Ok(#(ast.Bool(False), rest))
    [token.LBrace, ..rest] -> parse_block(rest, [])
    [tok, ..] -> error.unexpected("Unexpected token: " <> string.inspect(tok))
  }
}

pub fn parse_block(tokens: Tokens, acc: List(ast.Expr)) -> Parsed {
  case tokens {
    [token.RBrace, ..rest] -> {
      case acc {
        [] -> Error(error.EmptyBlock)
        _ -> Ok(#(ast.Block(list.reverse(acc)), rest))
      }
    }
    _ -> {
      use #(expr, rest) <- then(parse_expr(tokens))
      parse_block(rest, [expr, ..acc])
    }
  }
}
