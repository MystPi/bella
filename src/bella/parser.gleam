import gleam/string
import gleam/list
import gleam/io
import gleam/option.{None, Option, Some}
import nibble.{do, return}
import nibble/pratt
import bella/lexer/token.{TokenT}

// TYPES .......................................................................

pub type Expr {
  Var(String)
  String(String)
  Number(Float)
  Bool(Bool)
  Block(List(Expr))
  Record(fields: List(#(String, Expr)))
  RecordAccess(record: Expr, field: String)
  BinOp(operator: TokenT, left: Expr, right: Expr)
  Unary(operator: TokenT, value: Expr)
  Lambda(param: String, body: Expr)
  Lambda0(body: Expr)
  Let(name: String, value: Expr, body: Expr)
  Call(callee: Expr, arg: Expr)
  Call0(callee: Expr)
  If(cond: Expr, true_branch: Expr, false_branch: Expr)
  Try(body: Expr, else: Expr)
  Throw(value: Expr)
}

pub type Module {
  Module(imports: List(Import), body: List(Expr))
}

type Import =
  #(String, String)

// PARSER ......................................................................

pub fn parse(tokens: token.Tokens) {
  io.debug(nibble.run(tokens, module_parser()))
}

fn module_parser() {
  use imports <- do(nibble.many(import_parser()))
  use exprs <- do(nibble.many1(expr_parser()))

  return(Module(imports: imports, body: exprs))
}

fn import_parser() {
  use _ <- do(nibble.token(token.Import))
  use root <- do(ident_parser())
  use names <- do(nibble.many({
    use _ <- do(nibble.token(token.Slash))
    use name <- do(ident_parser())
    return(name)
  }))
  use alias <- do(
    {
      use _ <- do(nibble.token(token.As))
      use alias <- do(ident_parser())
      return(alias)
    }
    |> nibble.or(root),
  )

  return(#(alias, string.join([root, ..names], "/")))
}

fn expr_parser() {
  call_parser()
}

fn call_parser() {
  use callee <- do(primary_parser())
  use result <- do(
    nibble.one_of([
      {
        use _ <- do(nibble.token(token.LParen))
        use args <- do(nibble.list(expr_parser(), nibble.token(token.Comma)))
        use _ <- do(nibble.token(token.RParen))

        case args {
          [] -> return(Call0(callee))
          _ -> return(list.fold(args, callee, Call))
        }
      },
    ])
    |> nibble.or(callee),
  )

  return(result)
}

fn primary_parser() {
  nibble.one_of([
    ident_parser()
    |> nibble.map(Var),
    number_parser()
    |> nibble.map(Number),
    string_parser()
    |> nibble.map(String),
    bool_parser(),
    block_parser(),
  ])
}

fn bool_parser() {
  nibble.one_of([
    nibble.token(token.True)
    |> nibble.replace(Bool(True)),
    nibble.token(token.False)
    |> nibble.replace(Bool(False)),
  ])
}

fn block_parser() {
  use _ <- do(nibble.token(token.LParen))
  use exprs <- do(nibble.many1(expr_parser()))
  use _ <- do(nibble.token(token.RParen))

  return(Block(exprs))
}

// HELPERS .....................................................................

fn ident_parser() {
  use tok <- nibble.take_map("an identifier")

  case tok {
    token.Ident(n) -> Some(n)
    _ -> None
  }
}

fn number_parser() {
  use tok <- nibble.take_map("a number")

  case tok {
    token.Number(n) -> Some(n)
    _ -> None
  }
}

fn string_parser() {
  use tok <- nibble.take_map("a string")

  case tok {
    token.String(n) -> Some(n)
    _ -> None
  }
}
