import gleam/map
import gleam/io
import gleam/float
import gleam/int
import gleam/result.{try}
import bella/error
import bella/parser
import bella/utils
import bella/lexer
import bella/lexer/token

pub type DataType {
  Number(Float)
  String(String)
  Bool(Bool)
  Lambda(param: String, body: parser.Expr, closure: Scope)
  Builtin(func: fn(DataType, Scope) -> Evaluated)
}

type Scope =
  map.Map(String, DataType)

type Evaluated =
  Result(#(DataType, Scope), error.Error)

pub fn evaluate_str(str: String) -> Evaluated {
  use tokens <- try(lexer.lex(str))
  use parsed <- try(parser.parse(tokens))
  evaluate(parsed)
}

fn evaluate(exprs: List(parser.Expr)) -> Evaluated {
  let builtins =
    map.from_list([
      #(
        "print",
        Builtin(fn(x, scope) {
          io.println(to_string(x))
          Ok(#(x, scope))
        }),
      ),
      #(
        "to_string",
        Builtin(fn(x, scope) { Ok(#(String(to_string(x)), scope)) }),
      ),
      #("import", Builtin(import_file)),
    ])
  eval_all(exprs, [], builtins)
}

fn import_file(path: DataType, scope: Scope) -> Evaluated {
  // TODO: fix relative paths
  case path {
    String(path) ->
      case utils.read_file(path) {
        Ok(contents) ->
          case evaluate_str(contents) {
            Ok(#(result, _)) -> Ok(#(result, scope))
            _ as error -> error
          }
        _ ->
          error.runtime_error(
            "I couldn't find the requested file to import: " <> path,
          )
      }
    _ -> error.runtime_error("Import path must be a string")
  }
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
    parser.Var(x) -> get_var(x, scope)
    parser.Block(exprs) -> eval_all(exprs, [], scope)
    parser.BinOp(op, left, right) -> eval_binop(op, left, right, scope)
    parser.Unary(op, value) -> eval_unary(op, value, scope)
    parser.Call(callee, arg) -> {
      use #(callee, _) <- try(eval(callee, scope))
      use #(arg, _) <- try(eval(arg, scope))
      eval_call(callee, arg, scope)
    }
    parser.Let(name, value, body) -> eval_let(name, value, body, scope)
    parser.If(cond, true_branch, false_branch) ->
      eval_if(cond, true_branch, false_branch, scope)
  }
}

fn get_var(name: String, scope: Scope) -> Evaluated {
  case map.get(scope, name) {
    Ok(x) -> Ok(#(x, scope))
    _ -> error.runtime_error("There is no binding for `" <> name <> "`")
  }
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
    token.Plus -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a +. b), scope))
        String(a), String(b) -> Ok(#(String(a <> b), scope))
        _, _ ->
          error.runtime_error(
            "Operands of + must be numbers or strings and be the same type",
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
        _, _ -> error.runtime_error("Operands of - must be numbers")
      }
    }
    token.Star -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a *. b), scope))
        _, _ -> error.runtime_error("Operands of * must be numbers")
      }
    }
    token.Slash -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a /. b), scope))
        _, _ -> error.runtime_error("Operands of / must be numbers")
      }
    }
    token.Less -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a <. b), scope))
        _, _ -> error.runtime_error("Operands of < must be numbers")
      }
    }
    token.Greater -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a >. b), scope))
        _, _ -> error.runtime_error("Operands of > must be numbers")
      }
    }
    token.LessEq -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a <=. b), scope))
        _, _ -> error.runtime_error("Operands of <= must be numbers")
      }
    }
    token.GreaterEq -> {
      case left, right {
        Number(a), Number(b) -> Ok(#(Bool(a >=. b), scope))
        _, _ -> error.runtime_error("Operands of >= must be numbers")
      }
    }
    token.And -> {
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a && b), scope))
        _, _ -> error.runtime_error("Operands of `and` must be Booleans")
      }
    }
    token.Or -> {
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a || b), scope))
        _, _ -> error.runtime_error("Operands of `and` must be Booleans")
      }
    }
    token.RPipe -> {
      eval_call(right, left, scope)
    }
    _ -> error.runtime_error("BinOp not implemented")
  }
}

fn eval_unary(op: token.Token, value: parser.Expr, scope: Scope) -> Evaluated {
  case op {
    token.Minus -> {
      use #(value, _) <- try(eval(value, scope))
      case value {
        Number(x) -> Ok(#(Number(0.0 -. x), scope))
        _ -> error.runtime_error("Unary - applies to Numbers")
      }
    }
    token.Bang -> {
      use #(value, _) <- try(eval(value, scope))
      case value {
        Bool(x) -> Ok(#(Bool(!x), scope))
        _ -> error.runtime_error("Unary ! applies to Booleans")
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

fn to_string(x: DataType) -> String {
  case x {
    Number(n) ->
      case float.floor(n) == n {
        True -> int.to_string(float.truncate(n))
        False -> float.to_string(n)
      }
    String(s) -> s
    Bool(b) ->
      case b {
        True -> "true"
        False -> "false"
      }
    Lambda(param, ..) -> "#lambda<" <> param <> ">"
    Builtin(..) -> "#builtin"
  }
}
