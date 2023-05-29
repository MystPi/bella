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
    Ok(contents) -> run_str(contents, path)
    _ -> io.println_error("I couldn't find the requested file: " <> path)
  }
}

fn run_str(str: String, path: String) -> Nil {
  case evaluator.evaluate_str(str) {
    Error(err) -> error.print_error(err, str, path)
    _ -> Nil
  }
}
