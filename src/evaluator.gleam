import gleam/map
import gleam/io
import gleam/float
import gleam/int
import gleam/result.{then}
import data/error
import parser
import lexer

pub type DataType {
  Number(Float)
  String(String)
  Bool(Bool)
  Lambda(param: String, body: parser.Expr, closure: Scope)
  Builtin(func: fn(DataType, Scope) -> Evaluated)
}

pub type Scope =
  map.Map(String, DataType)

pub type Evaluated =
  Result(#(DataType, Scope), error.Error)

pub fn evaluate(exprs: List(parser.Expr)) -> Evaluated {
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
    ])
  eval_all(exprs, [], builtins)
}

pub fn eval_all(
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
      use #(result, _) <- then(eval(expr, scope))
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
    parser.Call(callee, arg) -> eval_call(callee, arg, scope)
    parser.Let(name, value, body) -> eval_let(name, value, body, scope)
    parser.If(cond, true_branch, false_branch) ->
      eval_if(cond, true_branch, false_branch, scope)
  }
}

fn get_var(name: String, scope: Scope) -> Evaluated {
  case map.get(scope, name) {
    Ok(x) -> Ok(#(x, scope))
    _ -> error.runtime_error("Var not found: " <> name)
  }
}

fn eval_binop(
  op: lexer.Token,
  left: parser.Expr,
  right: parser.Expr,
  scope: Scope,
) -> Evaluated {
  case op {
    lexer.Plus -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a +. b), scope))
        String(a), String(b) -> Ok(#(String(a <> b), scope))
        _, _ ->
          error.runtime_error(
            "Operands of + must be numbers or strings and be the same type",
          )
      }
    }
    lexer.Minus -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a -. b), scope))
        _, _ -> error.runtime_error("Operands of - must be numbers")
      }
    }
    lexer.Star -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a *. b), scope))
        _, _ -> error.runtime_error("Operands of * must be numbers")
      }
    }
    lexer.Slash -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      case left, right {
        Number(a), Number(b) -> Ok(#(Number(a /. b), scope))
        _, _ -> error.runtime_error("Operands of / must be numbers")
      }
    }
    lexer.EqEq -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      Ok(#(Bool(left == right), scope))
    }
    lexer.Neq -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      Ok(#(Bool(left != right), scope))
    }
    lexer.And -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a && b), scope))
        _, _ -> error.runtime_error("Operands of `and` must be Booleans")
      }
    }
    lexer.Or -> {
      use #(left, _) <- then(eval(left, scope))
      use #(right, _) <- then(eval(right, scope))
      case left, right {
        Bool(a), Bool(b) -> Ok(#(Bool(a || b), scope))
        _, _ -> error.runtime_error("Operands of `and` must be Booleans")
      }
    }
    lexer.RPipe -> {
      eval_call(right, left, scope)
    }
    _ -> error.runtime_error("BinOp not implemented")
  }
}

fn eval_unary(op: lexer.Token, value: parser.Expr, scope: Scope) -> Evaluated {
  case op {
    lexer.Minus -> {
      use #(value, _) <- then(eval(value, scope))
      case value {
        Number(x) -> Ok(#(Number(0.0 -. x), scope))
        _ -> error.runtime_error("Unary - applies to Numbers")
      }
    }
    lexer.Bang -> {
      use #(value, _) <- then(eval(value, scope))
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

fn eval_call(callee: parser.Expr, arg: parser.Expr, scope: Scope) -> Evaluated {
  use #(callee, _) <- then(eval(callee, scope))
  use #(arg, _) <- then(eval(arg, scope))
  case callee {
    Lambda(param, body, closure) -> {
      use #(result, _) <- then(eval(
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
  use #(value, _) <- then(eval(value, scope))
  use #(result, _) <- then(eval(body, create_var(name, value, scope)))
  Ok(#(result, scope))
}

fn eval_if(
  cond: parser.Expr,
  true_branch: parser.Expr,
  false_branch: parser.Expr,
  scope: Scope,
) -> Evaluated {
  use #(cond, _) <- then(eval(cond, scope))
  case cond {
    Bool(x) ->
      case x {
        True -> eval(true_branch, scope)
        False -> eval(false_branch, scope)
      }
    _ -> error.runtime_error("If only works on Booleans")
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
