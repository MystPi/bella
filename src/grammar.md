<!-- > **Note**: this is the _target_ grammar; the implementation currently has some differences: -->

```
module := import* expr+

import := 'import' Ident ( '/' Ident )* ( 'as' Ident )?

expr :=
  | pipe
  | lambda
  | let_expr
  | if_expr
  | throw_expr
  | try_expr
  | match_expr

lambda := Ident? '->' expr
let_expr := 'let' ( pattern '=' expr 'in' )+ expr
if_expr := 'if' expr 'then' expr 'else' expr
throw_expr := 'throw' expr
try_expr := 'try' expr 'else' expr
match_expr := 'match' match_clause+
match_clause := 'is' pattern 'then' expr

pipe := logic_or ( '|>' logic_or ) *
logic_or := logic_and ( 'or' logic_and )*
logic_and := equality ( 'and' equality )*
equality := comparison ( ( '==' | '!=' ) comparison )*
comparison := term ( ( '>' | '>=' | '<' | '<=' ) term )*
term := factor ( ( '+' | '-' ) factor )*
factor := named_pat ( ( '/' | '*' ) named_pat )*
named_pat := unary ( 'as' ident )?
unary := ( '-' | '!' | '^' | '?' ) unary | call
call := primary ( '(' comma_sep<expr>? ')' | '.' Ident )*

// Pattern is validated after parse
pattern := named_pat

primary :=
  | Ident
  | Number
  | String
  | bool
  | block
  | record
  | list

record := '{' comma_sep<record_item>? '}'
record_item := Ident ( ':' expr )?
list := '[' comma_sep<expr>? ('|' ident)? ']'
block := '(' expr+ ')'
bool := 'true' | 'false'

comma_sep<r> := r ( ',' r )*
```
