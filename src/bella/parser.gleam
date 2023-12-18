import gleam/list
import gleam/result.{try}
import bella/error
import bella/lexer/token.{
  type Span, type Token, type TokenType, type Tokens, Span,
}

// TYPES .......................................................................

pub type ExprAst {
  Var(String)
  String(String)
  Number(Float)
  Bool(Bool)
  Block(List(Expr))
  Record(fields: List(#(String, Expr)))
  RecordAccess(record: Expr, field: String)
  List(List(Expr))
  PatList(list: List(Expr), rest_name: String)
  BinOp(operator: Token, left: Expr, right: Expr)
  Unary(operator: Token, value: Expr)
  Lambda(param: String, body: Expr)
  Lambda0(body: Expr)
  Let(pattern: Expr, value: Expr, body: Expr)
  Call(callee: Expr, arg: Expr)
  Call0(callee: Expr)
  If(cond: Expr, true_branch: Expr, false_branch: Expr)
  Try(body: Expr, else: Expr)
  Throw(value: Expr)
  Match(value: Expr, cases: List(#(Expr, Expr)))
  NamedPat(value: Expr, name: String)
}

type Expr =
  #(ExprAst, Span)

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
    [#(token.If, from), ..rest] -> parse_if(rest, from)
    [#(token.Let, from), ..rest] -> parse_let(rest, from)
    [#(token.Ident(n), from), #(token.Arrow, _), ..rest] -> {
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(#(Lambda(n, body), span(from, body.1)), rest))
    }
    [#(token.Arrow, from), ..rest] -> {
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(#(Lambda0(body), span(from, body.1)), rest))
    }
    [#(token.Throw, from), ..rest] -> {
      use #(value, rest) <- try(parse_expr(rest))
      Ok(#(#(Throw(value), span(from, value.1)), rest))
    }
    [#(token.Try, from), ..rest] -> parse_try(rest, from)
    [#(token.Match, from), ..rest] -> parse_match(rest, from)
    _ -> parse_pipe(tokens)
  }
}

fn parse_let(tokens: Tokens, from: Span) -> Parsed {
  use #(pattern, rest) <- try(parse_pattern(tokens))
  use rest, _ <- expect(token.Eq, rest, "= after pattern")
  use #(value, rest) <- try(parse_expr(rest))

  finish_let(
    rest,
    fn(body) { #(Let(pattern, value, body), span(from, body.1)) },
    from,
  )
}

fn finish_let(
  tokens: Tokens,
  constructor: fn(Expr) -> Expr,
  from: Span,
) -> Parsed {
  case tokens {
    [#(token.In, _), ..rest] -> {
      use #(body, rest) <- try(parse_expr(rest))
      Ok(#(constructor(body), rest))
    }
    _ -> {
      use #(pattern, rest) <- try(parse_pattern(tokens))
      use rest, _ <- expect(token.Eq, rest, "= after pattern")
      use #(value, rest) <- try(parse_expr(rest))
      use #(body, rest) <- try(finish_let(
        rest,
        fn(body) { #(Let(pattern, value, body), span(from, body.1)) },
        from,
      ))
      Ok(#(constructor(body), rest))
    }
  }
}

fn parse_pattern(tokens: Tokens) -> Parsed {
  use #(pattern, rest) <- try(parse_named_pat(tokens))
  use _ <- try(validate_pattern(pattern))
  Ok(#(pattern, rest))
}

fn validate_pattern(pattern: Expr) -> Result(Nil, error.Error) {
  case pattern {
    #(Var(_), _)
    | #(String(_), _)
    | #(Number(_), _)
    | #(Bool(_), _)
    | #(Unary(#(token.Caret, _), _), _)
    | #(Unary(#(token.Question, _), _), _) -> Ok(Nil)
    #(NamedPat(p, _), _) -> validate_pattern(p)
    #(List(patterns), _) | #(PatList(patterns, _), _) ->
      list.try_map(patterns, validate_pattern)
      |> result.replace(Nil)
    #(Record(fields), _) ->
      {
        use #(_, field) <- list.try_map(fields)
        validate_pattern(field)
      }
      |> result.replace(Nil)

    #(_, pos) -> error.syntax_error("Invalid pattern", pos)
  }
}

fn parse_if(tokens: Tokens, from: Span) -> Parsed {
  use #(condition, rest) <- try(parse_expr(tokens))
  use rest, _ <- expect(token.Then, rest, "`then` after condition")
  use #(true_branch, rest) <- try(parse_expr(rest))
  use rest, _ <- expect(token.Else, rest, "`else` after true branch")
  use #(false_branch, rest) <- try(parse_expr(rest))
  Ok(#(
    #(If(condition, true_branch, false_branch), span(from, false_branch.1)),
    rest,
  ))
}

fn parse_try(tokens: Tokens, from: Span) -> Parsed {
  use #(body, rest) <- try(parse_expr(tokens))
  use rest, _ <- expect(token.Else, rest, "`else` after try body")
  use #(else, rest) <- try(parse_expr(rest))
  Ok(#(#(Try(body, else), span(from, else.1)), rest))
}

fn parse_match(tokens: Tokens, from: Span) -> Parsed {
  use #(value, rest) <- try(parse_expr(tokens))
  use rest, _ <- expect(token.Is, rest, "`is` after match value")
  use #(pattern, rest) <- try(parse_pattern(rest))
  use rest, _ <- expect(token.Then, rest, "`then` after match pattern")
  use #(body, rest) <- try(parse_expr(rest))
  finish_match(rest, value, [#(pattern, body)], from, body.1)
}

fn finish_match(
  tokens: Tokens,
  value: Expr,
  clauses: List(#(Expr, Expr)),
  from: Span,
  to: Span,
) -> Parsed {
  case tokens {
    [#(token.Is, _), ..rest] -> {
      use #(pattern, rest) <- try(parse_pattern(rest))
      use rest, _ <- expect(token.Then, rest, "`then` after match pattern")
      use #(body, rest) <- try(parse_expr(rest))
      finish_match(rest, value, [#(pattern, body), ..clauses], from, body.1)
    }
    _ -> Ok(#(#(Match(value, list.reverse(clauses)), span(from, to)), tokens))
  }
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
  parse_binop(tokens, [token.Star, token.Slash], parse_named_pat)
}

fn parse_named_pat(tokens: Tokens) -> Parsed {
  use #(expr, rest) <- try(parse_unary(tokens))

  case rest {
    [#(token.As, _), #(token.Ident(name), to), ..rest] -> {
      use _ <- try(validate_pattern(expr))
      Ok(#(#(NamedPat(expr, name), span(expr.1, to)), rest))
    }
    [#(token.As, _), #(_, pos), ..] ->
      error.syntax_error("I expected an identifier after `as`", pos)
    _ -> Ok(#(expr, rest))
  }
}

fn parse_unary(tokens: Tokens) -> Parsed {
  case tokens {
    [#(token.Minus, from) as op, ..rest]
    | [#(token.Bang, from) as op, ..rest]
    | [#(token.Caret, from) as op, ..rest]
    | [#(token.Question, from) as op, ..rest] -> {
      use #(expr, rest) <- try(parse_unary(rest))
      Ok(#(#(Unary(op, expr), span(from, expr.1)), rest))
    }
    _ -> parse_call(tokens)
  }
}

fn parse_call(tokens: Tokens) -> Parsed {
  use #(expr, rest) <- try(parse_primary(tokens))
  finish_call(rest, expr, expr.1)
}

fn finish_call(tokens: Tokens, callee: Expr, from: Span) -> Parsed {
  case tokens {
    [#(token.LParen, _), #(token.RParen, to), ..rest] -> {
      finish_call(rest, #(Call0(callee), span(from, to)), from)
    }
    [#(token.LParen, _), ..rest] -> {
      use #(arg, rest) <- try(parse_expr(rest))
      finish_call_args(rest, #(Call(callee, arg), from), from)
    }
    [#(token.Dot, _), ..rest] -> {
      case rest {
        [#(token.Ident(name), to), ..rest] -> {
          finish_call(rest, #(RecordAccess(callee, name), span(from, to)), from)
        }
        [#(_, pos), ..] ->
          error.syntax_error("I expected an identifier after .", pos)
      }
    }
    _ -> Ok(#(callee, tokens))
  }
}

fn finish_call_args(tokens: Tokens, callee: Expr, from: Span) -> Parsed {
  case tokens {
    [#(token.RParen, to), ..rest]
    | [#(token.Comma, _), #(token.RParen, to), ..rest] ->
      finish_call(rest, complete_call_spans(callee, to), from)
    [#(token.Comma, _), ..rest] -> {
      use #(arg, rest) <- try(parse_expr(rest))
      finish_call_args(rest, #(Call(callee, arg), from), from)
    }
    [#(_, pos), ..] -> error.syntax_error("I expected a , or )", pos)
  }
}

fn complete_call_spans(expr: Expr, to: Span) -> Expr {
  case expr {
    #(Call0(callee), from) -> #(
      Call0(complete_call_spans(callee, to)),
      span(from, to),
    )
    #(Call(callee, arg), from) -> #(
      Call(complete_call_spans(callee, to), arg),
      span(from, to),
    )
    _ -> expr
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
      finish_binop(
        rest,
        #(BinOp(op, left, right), span(left.1, right.1)),
        ops,
        subrule,
      )
    }
    False -> Ok(#(left, tokens))
  }
}

fn parse_primary(tokens: Tokens) -> Parsed {
  case tokens {
    [#(token.Number(x), pos), ..rest] -> Ok(#(#(Number(x), pos), rest))
    [#(token.String(x), pos), ..rest] -> Ok(#(#(String(x), pos), rest))
    [#(token.Ident(x), pos), ..rest] -> Ok(#(#(Var(x), pos), rest))
    [#(token.True, pos), ..rest] -> Ok(#(#(Bool(True), pos), rest))
    [#(token.False, pos), ..rest] -> Ok(#(#(Bool(False), pos), rest))
    [#(token.LParen, from), ..rest] -> parse_block(rest, [], from)
    [#(token.LBrace, from), ..rest] -> parse_record(rest, from)
    [#(token.LBracket, from), ..rest] -> parse_list(rest, from)
    [#(_, pos), ..] -> error.syntax_error("I wasn't expecting this", pos)
  }
}

fn parse_block(tokens: Tokens, acc: List(Expr), from: Span) -> Parsed {
  case tokens {
    [#(token.RParen, pos), ..rest] ->
      case acc {
        [] -> error.syntax_error("I expected an expression", pos)
        _ -> Ok(#(#(Block(list.reverse(acc)), span(from, pos)), rest))
      }
    _ -> {
      use #(expr, rest) <- try(parse_expr(tokens))
      parse_block(rest, [expr, ..acc], from)
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
      Ok(#(#(name, #(Var(name), pos), pos), rest))
    }
    [#(_, pos), ..] -> error.syntax_error("I expected an identifier", pos)
  }
}

fn parse_record(tokens: Tokens, from: Span) -> Parsed {
  case tokens {
    [#(token.RBrace, pos), ..rest] -> {
      Ok(#(#(Record([]), span(from, pos)), rest))
    }
    _ -> {
      use #(#(name, value, _), rest) <- try(parse_record_item(tokens))
      finish_record(rest, [#(name, value)], from)
    }
  }
}

fn finish_record(
  tokens: Tokens,
  fields: List(#(String, Expr)),
  from: Span,
) -> Parsed {
  case tokens {
    [#(token.RBrace, to), ..rest]
    | [#(token.Comma, _), #(token.RBrace, to), ..rest] ->
      Ok(#(#(Record(fields), span(from, to)), rest))
    [#(token.Comma, _), ..rest] -> {
      use #(#(name, value, pos), rest) <- try(parse_record_item(rest))
      case list.any(fields, fn(f) { f.0 == name }) {
        True -> error.syntax_error("Duplicate record field", pos)
        False -> finish_record(rest, [#(name, value), ..fields], from)
      }
    }
    [#(_, pos), ..] -> error.syntax_error("I expected a , or }", pos)
  }
}

fn parse_list(tokens: Tokens, from: Span) -> Parsed {
  case tokens {
    [#(token.RBracket, to), ..rest] -> Ok(#(#(List([]), span(from, to)), rest))
    _ -> {
      use #(expr, rest) <- try(parse_expr(tokens))
      finish_list(rest, [expr], from)
    }
  }
}

fn finish_list(tokens: Tokens, items: List(Expr), from: Span) -> Parsed {
  case tokens {
    [#(token.Bar, _), #(token.Ident(name), _), ..rest] -> {
      use rest, to <- expect(token.RBracket, rest, "] after identifier")
      Ok(#(#(PatList(list.reverse(items), name), span(from, to)), rest))
    }
    [#(token.Bar, _), #(_, pos), ..] ->
      error.syntax_error("I expected an identifier", pos)
    [#(token.RBracket, to), ..rest]
    | [#(token.Comma, _), #(token.RBracket, to), ..rest] ->
      Ok(#(#(List(list.reverse(items)), span(from, to)), rest))
    [#(token.Comma, _), ..rest] -> {
      use #(expr, rest) <- try(parse_expr(rest))
      finish_list(rest, [expr, ..items], from)
    }
    [#(_, pos), ..] -> error.syntax_error("I expected a , or ]", pos)
  }
}

// UTILS .......................................................................

pub fn span(from: Span, to: Span) -> Span {
  Span(from.from, to.to)
}

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
