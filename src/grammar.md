<!-- > **Note**: this is the _target_ grammar; the implementation currently has some differences: -->

```
module := import* expr+

import := 'import' Ident ( '/' Ident )* ( 'as' Ident )?

expr := pipe | lambda | let_expr | if_expr | throw_expr | try_expr

lambda := Ident? '->' expr
let_expr := 'let' ( Ident '=' expr 'in' )+ expr
if_expr := 'if' '(' expr ')' expr 'else' expr
throw_expr := 'throw' expr
try_expr := 'try' expr 'else' expr

pipe := logic_or ( '|>' logic_or ) *
logic_or := logic_and ( 'or' logic_and )*
logic_and := equality ( 'and' equality )*
equality := comparison ( ( '==' | '!=' ) comparison )*
comparison := term ( ( '>' | '>=' | '<' | '<=' ) term )*
term := factor ( ( '+' | '-' ) factor )*
factor := unary ( ( '/' | '*' ) unary )*
unary := ( '-' | '!' ) unary | call
call := primary ( '(' comma_sep? ')' | '.' Ident )*

primary := Ident | Number | String | bool | block | record | list

record := '{' ( record_item ( ',' record_item )* )? '}'
record_item := Ident ( ':' expr )?
list := '[' comma_sep? ']'
block := '(' expr+ ')'
bool := 'true' | 'false'

comma_sep := expr ( ',' expr )*
```
