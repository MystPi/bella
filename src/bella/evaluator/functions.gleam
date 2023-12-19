import gleam/io
import gleam/map
import bella/error
import bella/utils
import bella/evaluator/types.{Bool, Function, List, Record, String}

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
      #("is_type", Function(is_type)),
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

fn println(x, scope, _) {
  io.println(types.to_string(x))
  Ok(#(x, scope))
}

fn print(x, scope, _) {
  io.print(types.to_string(x))
  Ok(#(x, scope))
}

fn readline(message, scope, _) {
  let result = utils.readline(types.to_string(message))
  Ok(#(String(result), scope))
}

// TYPES .......................................................................

fn to_string(x, scope, _) {
  Ok(#(String(types.to_string(x)), scope))
}

fn typeof_(x, scope, _) {
  Ok(#(String(types.to_type(x)), scope))
}

fn inspect(x, scope, _) {
  Ok(#(String(types.inspect(x)), scope))
}

fn is_type(t, scope, _) {
  use x, _, pos <- nest_function(scope)

  case t {
    String(t) -> Ok(#(Bool(types.to_type(x) == t), scope))
    _ -> error.runtime_error_pos("Type must be a string", pos)
  }
}

// LIST ........................................................................

fn first(x, scope, pos) {
  case x {
    List([first, ..]) -> Ok(#(first, scope))
    List([]) ->
      error.runtime_error_pos("I got an empty list; there is no `first`", pos)
    _ ->
      error.runtime_error_pos(
        "I expected a list; instead got a " <> types.to_type(x),
        pos,
      )
  }
}

fn rest(x, scope, pos) {
  case x {
    List([_, ..rest]) -> Ok(#(List(rest), scope))
    List([]) ->
      error.runtime_error_pos("I got an empty list; there is no `rest`", pos)
    _ ->
      error.runtime_error_pos(
        "I expected a list; instead got a " <> types.to_type(x),
        pos,
      )
  }
}

// UTILS .......................................................................

fn nest_function(scope, func) {
  Ok(#(Function(func), scope))
}
