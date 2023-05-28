import gleam/io
import bella/evaluator/types.{Builtin, String}

pub const builtins = [
  #("print", Builtin(print)),
  #("to_string", Builtin(to_string)),
  #("typeof", Builtin(typeof_)),
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
