import gleam
import gleam/io
import hug
import gleam_community/ansi
import bella/lexer/token

// TYPES .......................................................................

pub type Error {
  SyntaxError(String, token.Span)
  RuntimeError(String)
  RuntimeErrorPos(String, token.Span)
  ImportedError(Error, String, String)
}

// CONSTRUCTORS ................................................................

pub fn syntax_error(msg: String, pos: token.Span) {
  gleam.Error(SyntaxError(msg, pos))
}

pub fn runtime_error(msg: String) {
  gleam.Error(RuntimeError(msg))
}

pub fn runtime_error_pos(msg: String, pos: token.Span) {
  gleam.Error(RuntimeErrorPos(msg, pos))
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
    SyntaxError(msg, span) ->
      ansi.red("✕ ") <> hug.error(
        source,
        in: path,
        from: #(span.from.line, span.from.col),
        to: #(span.to.line, span.to.col),
        message: "invalid syntax",
        hint: ansi.blue("? ") <> msg,
      )
    RuntimeErrorPos(msg, span) ->
      ansi.red("✕ ") <> hug.error(
        source,
        in: path,
        from: #(span.from.line, span.from.col),
        to: #(span.to.line, span.to.col),
        message: "runtime error",
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
