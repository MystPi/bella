import gleam/io
import gleam/result.{try}
import bella/evaluator
import bella/error
import bella/utils

const usage = "Usage:
  bella create <name>   Create a new project
  bella run             Run the current project
  bella <path>          Run the given file"

pub fn main() {
  case utils.get_args() {
    ["create", name, ..] ->
      case create_project(name) {
        Ok(_) -> io.println("Created project: " <> name)
        _ -> io.println_error("Couldn't create project")
      }
    ["run", ..] -> run_project()
    [path, ..] -> run_file(path)
    _ -> io.println_error(usage)
  }
}

fn run_project() {
  case utils.file_exists("bella.json") {
    True -> run_file("./src/main.bella")
    False -> io.println_error("Not inside a project")
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

fn create_project(name: String) -> Result(Nil, String) {
  use _ <- try(utils.create_directory("./" <> name <> "/src"))
  use _ <- try(utils.write_file(
    "./" <> name <> "/bella.json",
    "{\n  \"name\": \"" <> name <> "\"\n}",
  ))
  use _ <- try(utils.write_file(
    "./" <> name <> "/src/main.bella",
    "print('Hello, world!')",
  ))
  Ok(Nil)
}
