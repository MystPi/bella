<!-- > **Note**: this is the _target_ grammar; the implementation currently has some differences: -->

```
expr := pipe | lambda | let_expr | if_expr | throw_expr | try_expr

lambda := Ident '->' expr
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
call := primary ( '(' expr ( ',' expr )* ')' | '.' Ident )*

primary := Ident | Number | String | bool | block | record

record := '{' ( record_item ( ',' Ident ':' expr )* )? '}'
record_item := Ident ( ':' expr )?
block := '(' expr+ ')'
bool := 'true' | 'false'
```