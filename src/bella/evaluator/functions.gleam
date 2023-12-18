import gleam/io
import gleam/map
import bella/error
import bella/utils
import bella/evaluator/types.{Function, List, Record, String, Bool}

pub fn functions() {
  let bella_io =
    [
      #("println", Function(println)),
      #("print", Function(print)),
      #("readline", Function(readline)),
    ]
    |> map.from_list
    |> Record

  let bella_types =
    [
      #("to_string", Function(to_string)),
      #("typeof", Function(typeof_)),
      #("inspect", Function(inspect)),
      #("is_type", Function(is_type))
    ]
    |> map.from_list
    |> Record

  let bella_list =
    [#("first", Function(first)), #("rest", Function(rest))]
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

fn is_type(t, scope) {
  use x, _ <- nest_function(scope)

  case t {
    String(t) -> Ok(#(Bool(types.to_type(x) == t), scope))
    _ -> error.runtime_error("Type must be a string")
  }
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

// UTILS .......................................................................

fn nest_function(scope, func) {
  Ok(#(Function(func), scope))
}