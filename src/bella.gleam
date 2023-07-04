import gleam/io
import gleam/result.{try}
import gleam/iterator
import gleam/list
import gleam/javascript
import gleam/string
import gleam_community/ansi
import bella/evaluator
import bella/error
import bella/utils
import bella/project

// const usage = "Usage:
//   bella create <name>   Create a new project
//   bella run             Run the current project
//   bella <path>          Run the given file"

const usage = [
  ["create <name>", "Create a new project"],
  ["run", "Run the current project"],
  ["<path>", "Run the given file"],
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

fn get_project() -> #(Bool, Result(project.Project, String)) {
  case utils.read_file("bella.json") {
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
  case utils.read_file(path) {
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
    Ok(_) -> success("Created project: " <> name)
    _ -> error("Failed to create project")
  }
}

fn create_project_files(name: String) -> Result(String, String) {
  use _ <- try(utils.create_directory("./" <> name <> "/src"))
  use _ <- try(utils.write_file(
    "./" <> name <> "/bella.json",
    project.init(name),
  ))
  utils.write_file(
    "./" <> name <> "/src/" <> name <> ".bella",
    "print('Hello, world!')",
  )
}

// USAGE TABLE .................................................................

fn print_usage() {
  let min_distance = get_min_distance_for_usage()
  io.println("Usage:")
  iterator.range(0, list.length(usage))
  |> iterator.map(fn(i) {
    let current_cmd =
      result.unwrap(list.at(result.unwrap(list.at(usage, i), [""]), 0), "")
    let distance = min_distance - string.length(current_cmd) + 3
    let cmd_desc =
      result.unwrap(list.at(result.unwrap(list.at(usage, i), [""]), 1), "")
    io.println(
      "bella " <> current_cmd <> duplicate_string(" ", distance) <> cmd_desc,
    )
  })
  |> iterator.run()
  Nil
}

fn get_min_distance_for_usage() -> Int {
  let min_distance = javascript.make_reference(0)
  iterator.range(0, list.length(usage))
  |> iterator.map(fn(i) {
    let evaluation_distance =
      string.length(result.unwrap(
        list.at(result.unwrap(list.at(usage, i), [""]), 0),
        "",
      ))
    case evaluation_distance > javascript.dereference(min_distance) {
      True -> evaluation_distance
      False -> javascript.dereference(min_distance)
    }
  })
  |> iterator.map(fn(val) { javascript.set_reference(min_distance, val) })
  |> iterator.run()
  javascript.dereference(min_distance)
}

fn duplicate_string(string: String, x: Int) -> String {
  let base_str = javascript.make_reference(string)
  iterator.range(0, x - 1)
  |> iterator.map(fn(_) {
    javascript.set_reference(
      base_str,
      string <> javascript.dereference(base_str),
    )
  })
  |> iterator.run()
  javascript.dereference(base_str)
}

// UTILS .......................................................................

fn error(msg: String) -> Nil {
  io.println_error(ansi.red("✕ ") <> msg)
}

fn success(msg: String) -> Nil {
  io.println(ansi.green("✓ ") <> msg)
}
