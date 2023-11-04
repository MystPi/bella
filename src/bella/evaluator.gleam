import gleam/map
import gleam/list
import gleam/result.{try}
import simplifile
import bella/error
import bella/parser
import bella/lexer
import bella/lexer/token
import bella/evaluator/functions
import bella/evaluator/types.{
  type DataType, type Evaluated, type Scope, Bool, Function, Lambda, Lambda0,
  List, Number, Record, String,
}

// EVALUATOR ...................................................................

pub fn evaluate_str(str: String) -> Evaluated {
  use tokens <- try(lexer.lex(str))
  use parsed <- try(parser.parse(tokens))
  evaluate(parsed)
}

fn evaluate(mod: parser.Module) -> Evaluated {
  let functions = [
    #("import_", Function(import_file_function)),
    ..functions.functions()
  ]
  use imported <- try({
    use parser.Import(alias, path) <- list.try_map(mod.imports)
    use #(result, _) <- try(import_file("src/" <> path <> ".bella"))
    Ok(#(alias, result))
  })
  eval_all(
    mod.body,
    [],
    map.merge(map.from_list(functions), map.from_list(imported)),
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
    parser.Var(x, pos) -> get_var(x, scope, pos)
    parser.Block(exprs) -> eval_all(exprs, [], scope)
    parser.Record(fields) -> eval_record(fields, map.new(), scope)
    parser.RecordAccess(record, field, pos) ->
      eval_record_access(record, field, scope, pos)
    parser.List(items) -> eval_list(items, scope)
    parser.BinOp(op, left, right) -> eval_binop(op, left, right, scope)
    parser.Unary(op, value) -> eval_unary(op, value, scope)
    parser.Call(callee, arg, pos) -> {
      use #(callee, _) <- try(eval(callee, scope))
      use #(arg, _) <- try(eval(arg, scope))
      eval_call(callee, arg, scope, pos)
    }
    parser.Call0(callee, pos) -> {
      use #(callee, _) <- try(eval(callee, scope))
      eval_call0(callee, scope, pos)
    }
    parser.Let(name, value, body) -> eval_let(name, value, body, scope)
    parser.If(cond, true_branch, false_branch, pos) ->
      eval_if(cond, true_branch, false_branch, scope, pos)
    parser.Throw(value, pos) -> eval_throw(value, scope, pos)
    parser.Try(body, else) -> eval_try(body, else, scope)
  }
}

fn get_var(name: String, scope: Scope, pos: token.Span) -> Evaluated {
  case map.get(scope, name) {
    Ok(x) -> Ok(#(x, scope))
    _ ->
      error.runtime_error_pos("There is no binding for `" <> name <> "`", pos)
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
  pos: token.Span,
) -> Evaluated {
  use #(record, _) <- try(eval(record, scope))
  case record {
    Record(fields) ->
      case map.get(fields, field) {
        Ok(x) -> Ok(#(x, scope))
        _ ->
          error.runtime_error_pos("Record has no `" <> field <> "` field", pos)
      }
    _ ->
      error.runtime_error_pos(
        "Expression has no `" <> field <> "` field because it is not a record",
        pos,
      )
  }
}

fn eval_list(items: List(parser.Expr), scope: Scope) -> Evaluated {
  use items <- try({
    use item <- list.try_map(items)
    use #(item, _) <- try(eval(item, scope))
    Ok(item)
  })
  Ok(#(List(items), scope))
}

fn eval_binop(
  op: token.Token,
  left: parser.Expr,
  right: parser.Expr,
  scope: Scope,
) -> Evaluated {
  use #(left, _) <- try(eval(left, scope))
  use #(right, _) <- try(eval(right, scope))
  case op {
    #(token.Plus, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a +. b), scope))
        String(a), String(b) -> Ok(#(String(a <> b), scope))
        Record(a), Record(b) -> Ok(#(Record(map.merge(a, b)), scope))
        List(a), List(b) -> Ok(#(List(list.append(a, b)), scope))
        _, _ ->
          op_error(
            "+",
            "numbers, strings, records, or lists and be the same type",
            op.1,
          )
      }

    #(token.EqEq, _) -> Ok(#(Bool(left == right), scope))

    #(token.Neq, _) -> Ok(#(Bool(left != right), scope))

    #(token.Minus, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a -. b), scope))
        _, _ -> op_error("-", "numbers", op.1)
      }

    #(token.Star, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a *. b), scope))
        _, _ -> op_error("*", "numbers", op.1)
      }

    #(token.Slash, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a /. b), scope))
        _, _ -> op_error("/", "numbers", op.1)
      }

    #(token.Less, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a <. b), scope))
        _, _ -> op_error("<", "numbers", op.1)
      }

    #(token.Greater, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a >. b), scope))
        _, _ -> op_error(">", "numbers", op.1)
      }

    #(token.LessEq, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a <=. b), scope))
        _, _ -> op_error("<=", "numbers", op.1)
      }

    #(token.GreaterEq, _) ->
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a >=. b), scope))
        _, _ -> op_error(">=", "numbers", op.1)
      }

    #(token.And, _) ->
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a && b), scope))
        _, _ -> op_error("`and`", "Booleans", op.1)
      }

    #(token.Or, _) ->
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a || b), scope))
        _, _ -> op_error("`or`", "Booleans", op.1)
      }

    #(token.RPipe, pos) -> eval_call(right, left, scope, pos)

    _ -> error.runtime_error("BinOp not implemented")
  }
}

fn eval_unary(op: token.Token, value: parser.Expr, scope: Scope) -> Evaluated {
  case op {
    #(token.Minus, _) -> {
      use #(value, _) <- try(eval(value, scope))
      case value {
        Number(x) -> Ok(#(Number(0.0 -. x), scope))
        x ->
          error.runtime_error_pos(
            "Unary - applies to numbers; instead got a " <> types.to_type(x),
            op.1,
          )
      }
    }
    #(token.Bang, _) -> {
      use #(value, _) <- try(eval(value, scope))
      case value {
        Bool(x) -> Ok(#(Bool(!x), scope))
        x ->
          error.runtime_error_pos(
            "Unary ! applies to Booleans; instead got a " <> types.to_type(x),
            op.1,
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

fn eval_call(
  callee: DataType,
  arg: DataType,
  scope: Scope,
  pos: token.Span,
) -> Evaluated {
  case callee {
    Lambda(param, body, closure) -> {
      use #(result, _) <- try(eval(
        body,
        create_var(param, arg, map.merge(scope, closure)),
      ))
      Ok(#(result, scope))
    }
    Function(func) -> func(arg, scope)
    Lambda0(..) ->
      error.runtime_error_pos("Lambda must be called without an argument", pos)
    _ ->
      error.runtime_error_pos(
        "`" <> types.inspect(callee) <> "` cannot be called\nThis could be caused from calling something with too many arguments.\nFor example, `add(1, 2, 3)` desugars to `add(1)(2)(3)` which reduces to `3(3)`",
        pos,
      )
  }
}

fn eval_call0(callee: DataType, scope: Scope, pos: token.Span) -> Evaluated {
  case callee {
    Lambda0(body, closure) -> {
      use #(result, _) <- try(eval(body, map.merge(scope, closure)))
      Ok(#(result, scope))
    }
    Lambda(n, ..) ->
      error.runtime_error_pos(
        "Lambda must be called with an argument, `" <> n <> "`",
        pos,
      )
    Function(..) ->
      error.runtime_error_pos("Function must be called with an argument", pos)
    _ ->
      error.runtime_error_pos(
        "`" <> types.inspect(callee) <> "` cannot be called",
        pos,
      )
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
  pos: token.Span,
) -> Evaluated {
  use #(cond, _) <- try(eval(cond, scope))
  case cond {
    Bool(x) ->
      case x {
        True -> eval(true_branch, scope)
        False -> eval(false_branch, scope)
      }
    _ ->
      error.runtime_error_pos("The condition for `if` must be a Boolean", pos)
  }
}

fn eval_throw(value: parser.Expr, scope: Scope, pos: token.Span) -> Evaluated {
  // TODO: allow any DataType to be thrown (requires new error type)
  use #(value, _) <- try(eval(value, scope))
  case value {
    String(s) -> error.runtime_error_pos(s, pos)
    _ -> error.runtime_error_pos("Can only `throw` strings", pos)
  }
}

fn eval_try(body: parser.Expr, else: parser.Expr, scope: Scope) -> Evaluated {
  case eval(body, scope) {
    Ok(#(x, _)) -> Ok(#(x, scope))
    Error(error.RuntimeError(msg)) | Error(error.RuntimeErrorPos(msg, _)) -> {
      case else {
        parser.Lambda(..) -> {
          use #(else, _) <- try(eval(else, scope))
          eval_call(
            else,
            String(msg),
            scope,
            // Pass a 'useless' span to `eval_call` since it is guaranteed not to
            // error from the arguments passed to it
            token.useless_span,
          )
        }
        _ -> eval(else, scope)
      }
    }
  }
}

// UTILS .......................................................................

fn op_error(op: String, must_be: String, pos: token.Span) -> Evaluated {
  error.runtime_error_pos("Operands of " <> op <> " must be " <> must_be, pos)
}

fn import_file(path: String) -> Evaluated {
  case simplifile.read(path) {
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

fn import_file_function(path: DataType, _: Scope) -> Evaluated {
  case path {
    String(path) -> import_file(path)
    _ -> error.runtime_error("Import path must be a string")
  }
}
