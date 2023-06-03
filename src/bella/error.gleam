import gleam
import gleam/io
import hug
import gleam_community/ansi

// TYPES .......................................................................

pub type Error {
  SyntaxError(String, #(Int, Int), #(Int, Int))
  RuntimeError(String)
  ImportedError(Error, String, String)
}

// CONSTRUCTORS ................................................................

pub fn syntax_error(msg: String, from: #(Int, Int), to: #(Int, Int)) {
  gleam.Error(SyntaxError(msg, from, to))
}

pub fn runtime_error(msg: String) {
  gleam.Error(RuntimeError(msg))
}

pub fn imported_error(err: Error, source: String, path: String) {
  gleam.Error(ImportedError(err, source, path))
}

// UTILS .......................................................................

pub fn print_error(err: Error, source: String, path: String) -> Nil {
  case err {
    ImportedError(err, source, path2) -> {
      "↓ imported from " <> ansi.italic(path)
      |> ansi.dim
      |> io.println_error
      print_error(err, source, path2)
    }
    _ -> print_error_message(err, source, path)
  }
}

pub fn print_error_message(err: Error, source: String, path: String) -> Nil {
  case err {
    SyntaxError(msg, start, end) ->
      ansi.red("✕ ") <> hug.error(
        source,
        in: path,
        from: start,
        to: end,
        message: "invalid syntax",
        hint: ansi.blue("? ") <> msg,
      )
    RuntimeError(msg) ->
      ansi.red("✕ runtime error") <> " in " <> ansi.blue(path) <> "\n" <> ansi.blue(
        "? ",
      ) <> msg
    _ -> "unknown error"
  }
  |> io.println_error
}
