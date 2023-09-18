import gleam/io
import gleam/list
import gleam/string
import gleam/int
import gleam/result.{try}
import gleam_community/ansi
import simplifile
import bella/evaluator
import bella/error
import bella/utils
import bella/project

const usage = [
  #("create <name>", "Create a new project"),
  #("run", "Run the current project"),
  #("<path>", "Run the given file"),
]

pub fn main() {
  case utils.get_args(), get_project() {
    _, #(True, Error(msg)) -> error(msg)
    ["create", name, ..], _ -> create_project(name)
    ["run", ..], #(True, Ok(project)) -> run_project(project.name)
    [path, ..], _ -> run_file(path)
    _, _ -> print_usage()
  }
}

fn print_usage() -> Nil {
  let assert Ok(max_len) =
    usage
    |> list.map(fn(x) { string.length(x.0) })
    |> list.sort(int.compare)
    |> list.last

  io.println("Usage:")

  usage
  |> list.each(fn(x) {
    let #(command, desc) = x
    let left_over = max_len - string.length(command) + 2
    io.println("  bella " <> command <> string.repeat(" ", left_over) <> desc)
  })
}

fn get_project() -> #(Bool, Result(project.Project, String)) {
  case simplifile.read("bella.json") {
    Ok(contents) ->
      case project.decode(contents) {
        Ok(project) -> #(True, Ok(project))
        _ -> #(True, Error("Invalid bella.json"))
      }
    _ -> #(False, Error("Not in a project"))
  }
}

fn run_project(name: String) -> Nil {
  run_file("src/" <> name <> ".bella")
}

fn run_file(path: String) -> Nil {
  case simplifile.read(path) {
    Ok(contents) -> run_str(contents, path)
    _ -> error("I couldn't find the requested file: " <> path)
  }
}

fn run_str(str: String, path: String) -> Nil {
  case evaluator.evaluate_str(str) {
    Error(err) -> error.print_error(err, str, path)
    _ -> Nil
  }
}

fn create_project(name: String) -> Nil {
  case create_project_files(name) {
    Ok(_) -> success("Created project in ./" <> name <> "/")
    _ -> error("Failed to create project")
  }
}

fn create_project_files(name: String) -> Result(Nil, simplifile.FileError) {
  use _ <- try(simplifile.create_directory_all("./" <> name <> "/src"))
  use _ <- try(simplifile.write(
    project.init(name),
    "./" <> name <> "/bella.json",
  ))
  simplifile.write(
    "print('Hello, world!')",
    "./" <> name <> "/src/" <> name <> ".bella",
  )
}

// UTILS .......................................................................

fn error(msg: String) -> Nil {
  io.println_error(ansi.red("✕ ") <> msg)
}

fn success(msg: String) -> Nil {
  io.println(ansi.green("✓ ") <> msg)
}
