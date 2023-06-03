import gleam/map
import gleam/list
import gleam/io
import gleam/result.{try}
import bella/error
import bella/parser
import bella/utils
import bella/lexer
import bella/lexer/token
import bella/evaluator/builtins
import bella/evaluator/types.{
  Bool, Builtin, DataType, Evaluated, Lambda, Lambda0, Number, Record, Scope,
  String,
}

// EVALUATOR ...................................................................

pub fn evaluate_str(str: String) -> Evaluated {
  use tokens <- try(lexer.lex(str))
  io.debug(tokens)
  parser.parse(tokens)
  Ok(#(Number(1.0), map.new()))
}

fn evaluate(mod: parser.Module) -> Evaluated {
  let builtins = [
    #("import_", Builtin(import_file_builtin)),
    ..builtins.builtins
  ]
  use imported <- try({
    use #(alias, path) <- list.try_map(mod.imports)
    use #(result, _) <- try(import_file("src/" <> path <> ".bella"))
    Ok(#(alias, result))
  })
  eval_all(
    mod.body,
    [],
    map.merge(map.from_list(builtins), map.from_list(imported)),
  )
}

fn eval_all(
  exprs: List(parser.Expr),
  evaled: List(DataType),
  scope: Scope,
) -> Evaluated {
  case exprs {
    [] ->
      case evaled {
        [first, ..] -> Ok(#(first, scope))
        // Already enforced by the parser... but, why not?
        _ -> error.runtime_error("Exprs cannot be empty")
      }
    [expr, ..rest] -> {
      use #(result, _) <- try(eval(expr, scope))
      eval_all(rest, [result, ..evaled], scope)
    }
  }
}

fn eval(expr: parser.Expr, scope: Scope) -> Evaluated {
  case expr {
    parser.Number(x) -> Ok(#(Number(x), scope))
    parser.String(x) -> Ok(#(String(x), scope))
    parser.Bool(x) -> Ok(#(Bool(x), scope))
    parser.Lambda(arg, body) -> Ok(#(Lambda(arg, body, scope), scope))
    parser.Lambda0(body) -> Ok(#(Lambda0(body, scope), scope))
    parser.Var(x) -> get_var(x, scope)
    parser.Block(exprs) -> eval_all(exprs, [], scope)
    parser.Record(fields) -> eval_record(fields, map.new(), scope)
    parser.RecordAccess(record, field) ->
      eval_record_access(record, field, scope)
    parser.BinOp(op, left, right) -> eval_binop(op, left, right, scope)
    parser.Unary(op, value) -> eval_unary(op, value, scope)
    parser.Call(callee, arg) -> {
      use #(callee, _) <- try(eval(callee, scope))
      use #(arg, _) <- try(eval(arg, scope))
      eval_call(callee, arg, scope)
    }
    parser.Call0(callee) -> {
      use #(callee, _) <- try(eval(callee, scope))
      eval_call0(callee, scope)
    }
    parser.Let(name, value, body) -> eval_let(name, value, body, scope)
    parser.If(cond, true_branch, false_branch) ->
      eval_if(cond, true_branch, false_branch, scope)
    parser.Throw(value) -> eval_throw(value, scope)
    parser.Try(body, else) -> eval_try(body, else, scope)
  }
}

fn get_var(name: String, scope: Scope) -> Evaluated {
  case map.get(scope, name) {
    Ok(x) -> Ok(#(x, scope))
    _ -> error.runtime_error("There is no binding for `" <> name <> "`")
  }
}

fn eval_record(
  fields: List(#(String, parser.Expr)),
  result: map.Map(String, DataType),
  scope: Scope,
) -> Evaluated {
  case fields {
    [] -> Ok(#(Record(result), scope))
    [#(name, value), ..rest] -> {
      use #(value, _) <- try(eval(value, scope))
      eval_record(rest, map.insert(result, name, value), scope)
    }
  }
}

fn eval_record_access(
  record: parser.Expr,
  field: String,
  scope: Scope,
) -> Evaluated {
  use #(record, _) <- try(eval(record, scope))
  case record {
    Record(fields) ->
      case map.get(fields, field) {
        Ok(x) -> Ok(#(x, scope))
        _ -> error.runtime_error("Record has no `" <> field <> "` field")
      }
    _ ->
      error.runtime_error(
        "Expression has no `" <> field <> "` field because it is not a record",
      )
  }
}

fn eval_binop(
  op: token.TokenT,
  left: parser.Expr,
  right: parser.Expr,
  scope: Scope,
) -> Evaluated {
  use #(left, _) <- try(eval(left, scope))
  use #(right, _) <- try(eval(right, scope))
  case op {
    token.Plus -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a +. b), scope))
        String(a), String(b) -> Ok(#(String(a <> b), scope))
        Record(a), Record(b) -> Ok(#(Record(map.merge(a, b)), scope))
        a, b ->
          op_error(
            "+",
            "numbers, strings, or records and be the same type",
            a,
            b,
          )
      }
    }
    token.EqEq -> {
      Ok(#(Bool(left == right), scope))
    }
    token.Neq -> {
      Ok(#(Bool(left != right), scope))
    }
    token.Minus -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a -. b), scope))
        a, b -> op_error("-", "numbers", a, b)
      }
    }
    token.Star -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a *. b), scope))
        a, b -> op_error("*", "numbers", a, b)
      }
    }
    token.Slash -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a /. b), scope))
        a, b -> op_error("/", "numbers", a, b)
      }
    }
    token.Less -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a <. b), scope))
        a, b -> op_error("<", "numbers", a, b)
      }
    }
    token.Greater -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a >. b), scope))
        a, b -> op_error(">", "numbers", a, b)
      }
    }
    token.LessEq -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a <=. b), scope))
        a, b -> op_error("<=", "numbers", a, b)
      }
    }
    token.GreaterEq -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a >=. b), scope))
        a, b -> op_error(">=", "numbers", a, b)
      }
    }
    token.And -> {
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a && b), scope))
        a, b -> op_error("`and`", "Booleans", a, b)
      }
    }
    token.Or -> {
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a || b), scope))
        a, b -> op_error("`or`", "Booleans", a, b)
      }
    }
    token.RPipe -> {
      eval_call(right, left, scope)
    }
    _ -> error.runtime_error("BinOp not implemented")
  }
}

fn eval_unary(op: token.TokenT, value: parser.Expr, scope: Scope) -> Evaluated {
  case op {
    token.Minus -> {
      use #(value, _) <- try(eval(value, scope))
      case value {
        Number(x) -> Ok(#(Number(0.0 -. x), scope))
        x ->
          error.runtime_error(
            "Unary - applies to numbers; instead got a " <> types.to_type(x),
          )
      }
    }
    token.Bang -> {
      use #(value, _) <- try(eval(value, scope))
      case value {
        Bool(x) -> Ok(#(Bool(!x), scope))
        x ->
          error.runtime_error(
            "Unary ! applies to Booleans; instead got a " <> types.to_type(x),
          )
      }
    }
    // Make the compiler happy :)
    _ -> error.runtime_error("Unary operator not implemented")
  }
}

fn create_var(name: String, value: DataType, scope: Scope) -> Scope {
  map.insert(scope, name, value)
}

fn eval_call(callee: DataType, arg: DataType, scope: Scope) -> Evaluated {
  case callee {
    Lambda(param, body, closure) -> {
      use #(result, _) <- try(eval(
        body,
        create_var(param, arg, map.merge(scope, closure)),
      ))
      Ok(#(result, scope))
    }
    Builtin(func) -> func(arg, scope)
    Lambda0(..) ->
      error.runtime_error("Lambda must be called without an argument")
    _ -> error.runtime_error("Expression cannot be called")
  }
}

fn eval_call0(calle: DataType, scope: Scope) -> Evaluated {
  case calle {
    Lambda0(body, closure) -> {
      use #(result, _) <- try(eval(body, map.merge(scope, closure)))
      Ok(#(result, scope))
    }
    Lambda(n, ..) ->
      error.runtime_error(
        "Lambda must be called with an argument, `" <> n <> "`",
      )
    Builtin(..) ->
      error.runtime_error("Builtin must be called with an argument")
    _ -> error.runtime_error("Expression cannot be called")
  }
}

fn eval_let(
  name: String,
  value: parser.Expr,
  body: parser.Expr,
  scope: Scope,
) -> Evaluated {
  use #(value, _) <- try(eval(value, scope))
  use #(result, _) <- try(eval(body, create_var(name, value, scope)))
  Ok(#(result, scope))
}

fn eval_if(
  cond: parser.Expr,
  true_branch: parser.Expr,
  false_branch: parser.Expr,
  scope: Scope,
) -> Evaluated {
  use #(cond, _) <- try(eval(cond, scope))
  case cond {
    Bool(x) ->
      case x {
        True -> eval(true_branch, scope)
        False -> eval(false_branch, scope)
      }
    _ -> error.runtime_error("The condition for `if` must be a Boolean")
  }
}

fn eval_throw(value: parser.Expr, scope: Scope) -> Evaluated {
  // TODO: allow any DataType to be thrown (requires new error type)
  use #(value, _) <- try(eval(value, scope))
  case value {
    String(s) -> error.runtime_error(s)
    _ -> error.runtime_error("Can only `throw` strings")
  }
}

fn eval_try(body: parser.Expr, else: parser.Expr, scope: Scope) -> Evaluated {
  case eval(body, scope) {
    Ok(#(x, _)) -> Ok(#(x, scope))
    Error(error.RuntimeError(msg)) -> {
      case else {
        parser.Lambda(..) -> {
          use #(else, _) <- try(eval(else, scope))
          eval_call(else, String(msg), scope)
        }
        _ -> eval(else, scope)
      }
    }
  }
}

// UTILS .......................................................................

fn op_error(op: String, must_be: String, a: DataType, b: DataType) -> Evaluated {
  error.runtime_error(
    "Operands of " <> op <> " must be " <> must_be <> "; instead got a " <> types.to_type(
      a,
    ) <> " and a " <> types.to_type(b),
  )
}

fn import_file(path: String) -> Evaluated {
  case utils.read_file(path) {
    Ok(contents) ->
      case evaluate_str(contents) {
        Ok(#(result, _)) -> Ok(#(result, map.new()))
        Error(err) -> error.imported_error(err, contents, path)
      }
    _ ->
      error.runtime_error(
        "I couldn't find the requested file to import: " <> path,
      )
  }
}

fn import_file_builtin(path: DataType, _: Scope) -> Evaluated {
  case path {
    String(path) -> import_file(path)
    _ -> error.runtime_error("Import path must be a string")
  }
}
