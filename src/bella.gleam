import gleam/io
import bella/evaluator
import bella/error
import bella/utils

pub fn main() {
  case utils.get_args() {
    [path, ..] -> run_file(path)
    _ -> io.println_error("Please specify a path")
  }
}

fn run_file(path: String) -> Nil {
  case utils.read_file(path) {
    Ok(contents) -> run_str(contents)
    _ -> io.println_error("I couldn't find the requested file: " <> path)
  }
}

fn run_str(str: String) -> Nil {
  case evaluator.evaluate_str(str) {
    Error(err) -> print_error(err)
    _ -> Nil
  }
}

fn print_error(err: error.Error) -> Nil {
  io.println_error(
    "-----------------------------------------------
There was a problem while running your program.
Here are the details:
-----------------------------------------------",
  )
  io.println_error(case err {
    error.Unexpected(msg) -> "I found an unexpected " <> msg
    error.Expected(msg) -> "I expected an " <> msg
    error.InvalidText(msg) -> "I don't know what this means: " <> msg
    error.RuntimeError(msg) -> msg
  })
}
