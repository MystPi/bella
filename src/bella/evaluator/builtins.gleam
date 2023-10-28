import gleam/io
import gleam/map
import bella/error
import bella/utils
import bella/evaluator/types.{Builtin, List, Record, String}

pub fn builtins() {
  let bella_io =
    [
      #("println", Builtin(println)),
      #("print", Builtin(print)),
      #("readline", Builtin(readline)),
    ]
    |> map.from_list
    |> Record

  let bella_types =
    [
      #("to_string", Builtin(to_string)),
      #("typeof", Builtin(typeof_)),
      #("inspect", Builtin(inspect)),
    ]
    |> map.from_list
    |> Record

  let bella_list =
    [#("first", Builtin(first)), #("rest", Builtin(rest))]
    |> map.from_list
    |> Record

  [#("Io", bella_io), #("Types", bella_types), #("List", bella_list)]
}

// IO ..........................................................................

fn println(x, scope) {
  io.println(types.to_string(x))
  Ok(#(x, scope))
}

fn print(x, scope) {
  io.print(types.to_string(x))
  Ok(#(x, scope))
}

fn readline(message, scope) {
  let result = utils.readline(types.to_string(message))
  Ok(#(String(result), scope))
}

// TYPES .......................................................................

fn to_string(x, scope) {
  Ok(#(String(types.to_string(x)), scope))
}

fn typeof_(x, scope) {
  Ok(#(String(types.to_type(x)), scope))
}

fn inspect(x, scope) {
  Ok(#(String(types.inspect(x)), scope))
}

// LIST ........................................................................

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
