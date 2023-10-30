import gleam/io
import bella/error
import bella/evaluator/types.{Function, List, String}

pub const functions = [
  #("print", Function(print)),
  #("to_string", Function(to_string)),
  #("typeof", Function(typeof_)),
  #("first", Function(first)),
  #("rest", Function(rest)),
]

fn print(x, scope) {
  io.println(types.to_string(x))
  Ok(#(x, scope))
}

fn to_string(x, scope) {
  Ok(#(String(types.to_string(x)), scope))
}

fn typeof_(x, scope) {
  Ok(#(String(types.to_type(x)), scope))
}

fn first(x, scope) {
  case x {
    List([first, ..]) -> Ok(#(first, scope))
    List([]) -> error.runtime_error("I got an empty list; there is no `first`")
    _ ->
      error.runtime_error(
        "I expected a list; instead got a " <> types.to_type(x),
      )
  }
}

fn rest(x, scope) {
  case x {
    List([_, ..rest]) -> Ok(#(List(rest), scope))
    List([]) -> error.runtime_error("I got an empty list; there is no `rest`")
    _ ->
      error.runtime_error(
        "I expected a list; instead got a " <> types.to_type(x),
      )
  }
}
