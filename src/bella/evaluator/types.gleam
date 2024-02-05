import gleam/map
import gleam/list
import gleam/float
import gleam/int
import gleam/string
import bella/error
import bella/parser
import bella/utils
import bella/lexer/token

// TYPES .......................................................................

pub type DataType {
  Number(Float)
  String(String)
  Bool(Bool)
  Record(fields: map.Map(String, DataType))
  List(List(DataType))
  Lambda(param: String, body: parser.Expr, closure: Scope)
  Lambda0(body: parser.Expr, closure: Scope)
  Function(func: fn(DataType, Scope, token.Span) -> Evaluated)
}

pub type Scope =
  map.Map(String, DataType)

pub type Evaluated =
  Result(#(DataType, Scope), error.Error)

// UTILS .......................................................................

pub fn to_string(x: DataType) -> String {
  case x {
    Number(n) ->
      case float.floor(n) == n {
        True -> int.to_string(float.truncate(n))
        False -> float.to_string(n)
      }
    String(s) -> s
    Bool(b) ->
      case b {
        True -> "true"
        False -> "false"
      }
    Record(f) -> record_to_string(f)
    List(l) -> list_to_string(l)
    Lambda(param, ..) -> "#lambda<" <> param <> ">"
    Lambda0(..) -> "#lambda<>"
    Function(..) -> "#function"
  }
}

pub fn inspect(x: DataType) -> String {
  case x {
    String(s) -> utils.stringify(s)
    _ -> to_string(x)
  }
}

fn list_to_string(items: List(DataType)) -> String {
  let items =
    items
    |> list.map(inspect)
    |> string.join(", ")

  "[" <> items <> "]"
}

fn record_to_string(fields: map.Map(String, DataType)) -> String {
  let fields =
    fields
    |> map.to_list
    |> list.map(fn(field) {
      let #(name, value) = field
      name <> ": " <> inspect(value)
    })
    |> string.join(", ")

  "{ " <> fields <> " }"
}

pub fn to_type(x: DataType) -> String {
  case x {
    Number(..) -> "number"
    String(..) -> "string"
    Bool(..) -> "boolean"
    Record(..) -> "record"
    List(..) -> "list"
    Lambda(..) | Lambda0(..) -> "lambda"
    Function(..) -> "function"
  }
}
