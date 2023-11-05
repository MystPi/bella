import gleam/list
import gleam/result.{try}
import bella/error
import bella/lexer/token.{
  type Span, type Token, type TokenType, type Tokens, Span,
}

// TYPES .......................................................................

pub type Expr {
  Var(String, pos: Span)
  String(String)
  Number(Float)
  Bool(Bool)
  Block(List(Expr))
  Record(fields: List(#(String, Expr)))
  RecordAccess(record: Expr, field: String, pos: Span)
  List(List(Expr))
  ListRest(list: List(Expr), rest_name: String, pos: Span)
  BinOp(operator: Token, left: Expr, right: Expr)
  Unary(operator: Token, value: Expr)
  Lambda(param: String, body: Expr)
  Lambda0(body: Expr)
  Let(pattern: Expr, value: Expr, body: Expr, pos: Span)
  Call(callee: Expr, arg: Expr, pos: Span)
  Call0(callee: Expr, pos: Span)
  If(cond: Expr, true_branch: Expr, false_branch: Expr, pos: Span)
  Try(body: Expr, else: Expr)
  Throw(value: Expr, pos: Span)
}

pub type Module {
  Module(imports: List(Import), body: List(Expr))
}

pub type Import {
  Import(alias: String, path: String)
}

type Parsed =
  Result(#(Expr, Tokens), error.Error)

type Parser =
  fn(Tokens) -> Parsed

// PARSER ......................................................................

pub fn parse(tokens: Tokens) -> Result(Module, error.Error) {
  parse_module(tokens)
}

fn parse_module(tokens: Tokens) -> Result(Module, error.Error) {
  use #(imports, rest) <- try(parse_imports(tokens, []))
  use exprs <- try(parse_exprs(rest, []))
  Ok(Module(imports: imports, body: exprs))
}

fn parse_imports(
  tokens: Tokens,
  imports: List(Import),
) -> Result(#(List(Import), Tokens), error.Error) {
  case tokens {
    [#(token.Import, _), #(token.Ident(name), _), ..rest] -> {
      use #(i, rest) <- try(finish_import(rest, name, name))
      case rest {
        [#(token.As, _), #(token.Ident(alias), _), ..rest] ->
          parse_imports(rest, [Import(alias, i.path), ..imports])
        [#(token.As, _), #(_, pos), ..] ->
          error.syntax_error("I expected an identifier", pos)
        _ -> parse_imports(rest, [i, ..imports])
      }
    }
    [#(token.Import, _), #(_, pos), ..] ->
      error.syntax_error("I expected an identifier", pos)
    _ -> Ok(#(list.reverse(imports), tokens))
  }
}

fn finish_import(
  tokens: Tokens,
  acc: String,
  name: String,
) -> Result(#(Import, Tokens), error.Error) {
  case tokens {
    [#(token.Slash, _), #(token.Ident(name), _), ..rest] ->
      finish_import(rest, acc <> "/" <> name, name)
    [#(token.Slash, _), #(_, pos), ..] ->
      error.syntax_error("I expected an identifier", pos)
    _ -> Ok(#(Import(name, acc), tokens))
  }
}

fn parse_exprs(
  tokens: Tokens,
  exprs: List(Expr),
) -> Result(List(Expr), error.Error) {
  case tokens {
    [#(token.Eof, pos)] ->
      case exprs {
        [] -> error.syntax_error("Unexpected end of file", pos)
        _ -> Ok(list.reverse(exprs))
      }
    _ -> {
      use #(expr, rest) <- try(parse_expr(tokens))
      parse_exprs(rest, [expr, ..exprs])
    }
  }
}

fn parse_expr(tokens: Tokens) -> Parsed {
  case tokens {
    [#(token.If, _), ..rest] -> parse_if(rest)
    [#(token.Let, _), ..rest] -> parse_let(rest)
    [#(token.Ident(n), _), #(token.Arrow, _), ..rest] -> {
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(Lambda(n, body), rest))
    }
    [#(token.Arrow, _), ..rest] -> {
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(Lambda0(body), rest))
    }
    [#(token.Throw, pos), ..rest] -> {
      use #(value, rest) <- try(parse_expr(rest))
      Ok(#(Throw(value, pos), rest))
    }
    [#(token.Try, _), ..rest] -> parse_try(rest)
    _ -> parse_pipe(tokens)
  }
}

fn parse_let(tokens: Tokens) -> Parsed {
  use #(pattern, rest) <- try(parse_pattern(tokens))
  use rest, pos <- expect(token.Eq, rest, "= after pattern")
  use #(value, rest) <- try(parse_expr(rest))

  finish_let(rest, Let(pattern, value, _, pos))
}

fn finish_let(tokens: Tokens, constructor: fn(Expr) -> Expr) {
  case tokens {
    [#(token.In, _), ..rest] -> {
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(constructor(body), rest))
    }
    _ -> {
      use #(pattern, rest) <- try(parse_pattern(tokens))
      use rest, pos <- expect(token.Eq, rest, "= after pattern")
      use #(value, rest) <- try(parse_expr(rest))
      use #(body, rest) <- try(finish_let(rest, Let(pattern, value, _, pos)))
      Ok(#(constructor(body), rest))
    }
  }
}

fn parse_pattern(tokens: Tokens) -> Parsed {
  use #(pattern, rest) <- try(parse_unary(tokens))
  use _ <- try(validate_pattern(pattern))
  Ok(#(pattern, rest))
}

fn validate_pattern(pattern: Expr) -> Result(Nil, error.Error) {
  case pattern {
    Var(..) | String(_) | Number(_) | Bool(_) | Unary(#(token.Caret, _), _) ->
      Ok(Nil)
    List(patterns) | ListRest(patterns, ..) ->
      list.try_map(patterns, validate_pattern)
      |> result.replace(Nil)
    Record(fields) ->
      {
        use #(_, field) <- list.try_map(fields)
        validate_pattern(field)
      }
      |> result.replace(Nil)
    // TODO: positions & use syntax_error
    _ -> error.runtime_error("Invalid pattern")
  }
}

fn parse_if(tokens: Tokens) -> Parsed {
  use rest, pos1 <- expect(token.LParen, tokens, "( before condition")
  use #(condition, rest) <- try(parse_expr(rest))
  use rest, pos2 <- expect(token.RParen, rest, ") after condition")
  use #(true_branch, rest) <- try(parse_expr(rest))
  use rest, _ <- expect(token.Else, rest, "`else` after true branch")
  use #(false_branch, rest) <- try(parse_expr(rest))
  Ok(#(If(condition, true_branch, false_branch, Span(pos1.from, pos2.to)), rest))
}

fn parse_try(tokens: Tokens) -> Parsed {
  use #(body, rest) <- try(parse_expr(tokens))
  use rest, _ <- expect(token.Else, rest, "`else` after try body")
  use #(else, rest) <- try(parse_expr(rest))
  Ok(#(Try(body, else), rest))
}

fn parse_pipe(tokens: Tokens) -> Parsed {
  parse_binop(tokens, [token.RPipe], parse_logic_or)
}

fn parse_logic_or(tokens: Tokens) -> Parsed {
  parse_binop(tokens, [token.Or], parse_logic_and)
}

fn parse_logic_and(tokens: Tokens) -> Parsed {
  parse_binop(tokens, [token.And], parse_equality)
}

fn parse_equality(tokens: Tokens) -> Parsed {
  parse_binop(tokens, [token.EqEq, token.Neq], parse_comparison)
}

fn parse_comparison(tokens: Tokens) -> Parsed {
  parse_binop(
    tokens,
    [token.Greater, token.GreaterEq, token.Less, token.LessEq],
    parse_term,
  )
}

fn parse_term(tokens: Tokens) -> Parsed {
  parse_binop(tokens, [token.Plus, token.Minus], parse_factor)
}

fn parse_factor(tokens: Tokens) -> Parsed {
  parse_binop(tokens, [token.Star, token.Slash], parse_unary)
}

fn parse_unary(tokens: Tokens) -> Parsed {
  case tokens {
    [#(token.Minus, _) as op, ..rest]
    | [#(token.Bang, _) as op, ..rest]
    | [#(token.Caret, _) as op, ..rest] -> {
      use #(expr, rest) <- try(parse_unary(rest))
      Ok(#(Unary(op, expr), rest))
    }
    _ -> parse_call(tokens)
  }
}

fn parse_call(tokens: Tokens) -> Parsed {
  use #(expr, rest) <- try(parse_primary(tokens))
  finish_call(rest, expr)
}

fn finish_call(tokens: Tokens, callee: Expr) -> Parsed {
  case tokens {
    [#(token.LParen, pos1), #(token.RParen, pos2), ..rest] -> {
      finish_call(rest, Call0(callee, Span(pos1.from, pos2.to)))
    }
    [#(token.LParen, pos), ..rest] -> {
      use #(arg, rest) <- try(parse_expr(rest))
      finish_call_args(rest, Call(callee, arg, pos))
    }
    [#(token.Dot, _), ..rest] -> {
      case rest {
        [#(token.Ident(name), pos), ..rest] -> {
          finish_call(rest, RecordAccess(callee, name, pos))
        }
        [#(_, pos), ..] ->
          error.syntax_error("I expected an identifier after .", pos)
      }
    }
    _ -> Ok(#(callee, tokens))
  }
}

fn finish_call_args(tokens: Tokens, callee: Expr) -> Parsed {
  case tokens {
    [#(token.RParen, _), ..rest]
    | [#(token.Comma, _), #(token.RParen, _), ..rest] ->
      finish_call(rest, callee)
    [#(token.Comma, pos), ..rest] -> {
      use #(arg, rest) <- try(parse_expr(rest))
      finish_call_args(rest, Call(callee, arg, pos))
    }
    [#(_, pos), ..] -> error.syntax_error("I expected a , or )", pos)
  }
}

fn parse_binop(tokens: Tokens, ops: List(TokenType), subrule: Parser) -> Parsed {
  use #(left, rest) <- try(subrule(tokens))
  finish_binop(rest, left, ops, subrule)
}

fn finish_binop(
  tokens: Tokens,
  left: Expr,
  ops: List(TokenType),
  subrule: Parser,
) -> Parsed {
  let [op, ..rest] = tokens
  case list.contains(ops, op.0) {
    True -> {
      use #(right, rest) <- try(subrule(rest))
      finish_binop(rest, BinOp(op, left, right), ops, subrule)
    }
    False -> Ok(#(left, tokens))
  }
}

fn parse_primary(tokens: Tokens) -> Parsed {
  case tokens {
    [#(token.Number(x), _), ..rest] -> Ok(#(Number(x), rest))
    [#(token.String(x), _), ..rest] -> Ok(#(String(x), rest))
    [#(token.Ident(x), pos), ..rest] -> Ok(#(Var(x, pos), rest))
    [#(token.True, _), ..rest] -> Ok(#(Bool(True), rest))
    [#(token.False, _), ..rest] -> Ok(#(Bool(False), rest))
    [#(token.LParen, _), ..rest] -> parse_block(rest, [])
    [#(token.LBrace, _), ..rest] -> parse_record(rest)
    [#(token.LBracket, _), ..rest] -> parse_list(rest)
    [#(_, pos), ..] -> error.syntax_error("I wasn't expecting this", pos)
  }
}

fn parse_block(tokens: Tokens, acc: List(Expr)) -> Parsed {
  case tokens {
    [#(token.RParen, pos), ..rest] ->
      case acc {
        [] -> error.syntax_error("I expected an expression", pos)
        _ -> Ok(#(Block(list.reverse(acc)), rest))
      }
    _ -> {
      use #(expr, rest) <- try(parse_expr(tokens))
      parse_block(rest, [expr, ..acc])
    }
  }
}

fn parse_record_item(tokens: Tokens) {
  case tokens {
    [#(token.Ident(name), pos), #(token.Colon, _), ..rest] -> {
      use #(value, rest) <- try(parse_expr(rest))
      Ok(#(#(name, value, pos), rest))
    }
    [#(token.Ident(name), pos), ..rest] -> {
      Ok(#(#(name, Var(name, pos), pos), rest))
    }
    [#(_, pos), ..] -> error.syntax_error("I expected an identifier", pos)
  }
}

fn parse_record(tokens: Tokens) -> Parsed {
  case tokens {
    [#(token.RBrace, _), ..rest] -> {
      Ok(#(Record([]), rest))
    }
    _ -> {
      use #(#(name, value, _), rest) <- try(parse_record_item(tokens))
      finish_record(rest, [#(name, value)])
    }
  }
}

fn finish_record(tokens: Tokens, fields: List(#(String, Expr))) -> Parsed {
  case tokens {
    [#(token.RBrace, _), ..rest]
    | [#(token.Comma, _), #(token.RBrace, _), ..rest] ->
      Ok(#(Record(fields), rest))
    [#(token.Comma, _), ..rest] -> {
      use #(#(name, value, pos), rest) <- try(parse_record_item(rest))
      case list.any(fields, fn(f) { f.0 == name }) {
        True -> error.syntax_error("Duplicate record field", pos)
        False -> finish_record(rest, [#(name, value), ..fields])
      }
    }
    [#(_, pos), ..] -> error.syntax_error("I expected a , or }", pos)
  }
}

fn parse_list(tokens: Tokens) -> Parsed {
  case tokens {
    [#(token.RBracket, _), ..rest] -> Ok(#(List([]), rest))
    _ -> {
      use #(expr, rest) <- try(parse_expr(tokens))
      finish_list(rest, [expr])
    }
  }
}

fn finish_list(tokens: Tokens, items: List(Expr)) -> Parsed {
  case tokens {
    [#(token.Bar, pos1), #(token.Ident(name), pos2), ..rest] -> {
      use rest, _ <- expect(token.RBracket, rest, "] after identifier")
      Ok(#(ListRest(list.reverse(items), name, Span(pos1.from, pos2.to)), rest))
    }
    [#(token.Bar, _), #(_, pos), ..] ->
      error.syntax_error("I expected an identifier", pos)
    [#(token.RBracket, _), ..rest]
    | [#(token.Comma, _), #(token.RBracket, _), ..rest] ->
      Ok(#(List(list.reverse(items)), rest))
    [#(token.Comma, _), ..rest] -> {
      use #(expr, rest) <- try(parse_expr(rest))
      finish_list(rest, [expr, ..items])
    }
    [#(_, pos), ..] -> error.syntax_error("I expected a , or ]", pos)
  }
}

// UTILS .......................................................................

fn expect(
  tok: TokenType,
  tokens: Tokens,
  msg: String,
  callback: fn(Tokens, Span) -> Parsed,
) -> Parsed {
  case tokens {
    [#(head, pos), ..tail] if head == tok -> callback(tail, pos)
    [#(_, pos), ..] -> error.syntax_error("I expected a " <> msg, pos)
  }
}
