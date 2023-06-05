import gleam/map
import gleam/list
import gleam/float
import gleam/int
import bella/error
import bella/parser

// TYPES .......................................................................

pub type DataType {
  Number(Float)
  String(String)
  Bool(Bool)
  Record(fields: map.Map(String, DataType))
  List(List(DataType))
  Lambda(param: String, body: parser.Expr, closure: Scope)
  Lambda0(body: parser.Expr, closure: Scope)
  Builtin(func: fn(DataType, Scope) -> Evaluated)
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
    Record(f) -> "#record<" <> int.to_string(map.size(f)) <> ">"
    List(l) -> "#list<" <> int.to_string(list.length(l)) <> ">"
    Lambda(param, ..) -> "#lambda<" <> param <> ">"
    Lambda0(..) -> "#lambda<>"
    Builtin(..) -> "#builtin"
  }
}

pub fn to_type(x: DataType) -> String {
  case x {
    Number(..) -> "number"
    String(..) -> "string"
    Bool(..) -> "boolean"
    Record(..) -> "record"
    List(..) -> "list"
    Lambda(..) | Lambda0(..) -> "lambda"
    Builtin(..) -> "builtin"
  }
}
