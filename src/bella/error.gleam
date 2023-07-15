import gleam
import gleam/io
import hug
import gleam_community/ansi
import bella/lexer/token

// TYPES .......................................................................

pub type Error {
  SyntaxError(String, token.Position)
  RuntimeError(String)
  ImportedError(Error, String, String)
}

// CONSTRUCTORS ................................................................

pub fn syntax_error(msg: String, pos: token.Position) {
  gleam.Error(SyntaxError(msg, pos))
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
      { "↓ imported from " <> ansi.italic(path) }
      |> ansi.dim
      |> io.println_error
      print_error(err, source, path2)
    }
    _ -> print_error_message(err, source, path)
  }
}

pub fn print_error_message(err: Error, source: String, path: String) -> Nil {
  case err {
    SyntaxError(msg, pos) ->
      ansi.red("✕ ") <> hug.error(
        source,
        in: path,
        from: pos.from,
        to: pos.to,
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
