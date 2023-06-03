import gleam/set
import gleam/int
import gleam/function
import nibble/lexer
import bella/error
import bella/lexer/token

pub fn lexer() {
  let keywords =
    set.from_list([
      "let", "in", "try", "throw", "if", "else", "or", "and", "import", "as",
      "true", "false",
    ])

  lexer.simple([
    lexer.identifier("[a-zA-Z]", "\\w", keywords, token.Ident),
    lexer.number_with_separator(",", int.to_float, function.identity)
    |> lexer.map(token.Number),
    lexer.string("\"", token.String),
    lexer.string("'", token.String),
    lexer.token("(", token.LParen),
    lexer.token(")", token.RParen),
    lexer.token("{", token.LBrace),
    lexer.token("}", token.RBrace),
    lexer.token("+", token.Plus),
    lexer.symbol("-", "[^>]", token.Minus),
    lexer.token("*", token.Star),
    lexer.token("/", token.Slash),
    lexer.symbol("<", "[^=]", token.Less),
    lexer.symbol(">", "[^=]", token.Greater),
    lexer.token("<=", token.LessEq),
    lexer.token(">=", token.GreaterEq),
    lexer.token("==", token.EqEq),
    lexer.token("!=", token.Neq),
    lexer.symbol("=", "[^=]", token.Eq),
    lexer.token(":", token.Colon),
    lexer.token("->", token.Arrow),
    lexer.token(",", token.Comma),
    lexer.token(".", token.Dot),
    lexer.token("|>", token.RPipe),
    lexer.symbol("!", "[^=]", token.Bang),
    lexer.keyword("let", "[\\W\\D]", token.Let),
    lexer.keyword("in", "[\\W\\D]", token.In),
    lexer.keyword("try", "[\\W\\D]", token.Try),
    lexer.keyword("throw", "[\\W\\D]", token.Throw),
    lexer.keyword("if", "[\\W\\D]", token.If),
    lexer.keyword("else", "[\\W\\D]", token.Else),
    lexer.keyword("or", "[\\W\\D]", token.Or),
    lexer.keyword("and", "[\\W\\D]", token.And),
    lexer.keyword("import", "[\\W\\D]", token.Import),
    lexer.keyword("as", "[\\W\\D]", token.As),
    lexer.keyword("true", "[\\W\\D]", token.True),
    lexer.keyword("false", "[\\W\\D]", token.False),
    lexer.comment(";", token.Comment)
    |> lexer.ignore,
    lexer.whitespace(Nil)
    |> lexer.ignore,
  ])
}

pub fn lex(source: String) {
  case lexer.run(source, lexer()) {
    Error(lexer.NoMatchFound(row, col, _)) ->
      error.syntax_error(
        "I don't know what this means",
        #(row, col),
        #(row, col + 1),
      )
    Ok(tokens) -> Ok(tokens)
  }
}
