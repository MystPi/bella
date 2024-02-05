# Language Tour

> *Note: this document is not completely up-to-date, most notably with the addition of pattern matching. The examples should still work, but not every aspect of Bella is covered.*

_[back to index](README.md)_

This document gives a quick overview of Bella's features. Basic knowledge of functional programming and programming in general is assumed.

## Hello World

```bella
Io.println("Hello world!")
```

## Comments

_[back to top](#language-tour)_

Comments are prefixed with a semicolon and continue until the end of the line.

```bella
; This is a comment
```

There are no multi-line comments in Bella.

## Values

_[back to top](#language-tour)_

The types of values Bella supports can be divided into two main categories: simple and complex. Simple values behave the same as in most programming languages.

- Strings: `"hello there"`, `'good bye'`
- Numbers: `5`, `42`, `3.14`
- Booleans: `true`, `false`

Complex values are, well, more complex, and can contain other values and [expressions](#expressions).

- [Lists](#lists): `[1, 2, 3, 4]`, `["a", true, 3]`
- [Records](#records): `{ name: "Bella", type: "dog" }`
- [Lambdas](#lambdas): `x -> Io.print(x)`, `a -> b -> a + b`

## Operators

_[back to top](#language-tour)_

- Arithemetic: `+`, `-`, `*`, `/`
- Equality: `==`, `!=`
- Comparison: `>`, `<`, `>=`, `<=`
- Logical: `and`, `or`
- Unary: `!` (not), `-` (negation)
- Record access: `.`
- Pipe: `|>`

| Precendence | Operator(s)       |
| ----------- | ----------------- |
| 9 (highest) | `.`               |
| 8           | `-` `!` (unary)   |
| 7           | `*` `/`           |
| 6           | `+` `-`           |
| 5           | `>` `<` `>=` `<=` |
| 4           | `==` `!=`         |
| 3           | `and`             |
| 2           | `or`              |
| 1 (lowest)  | `\|>`             |

## Variables (`let ... in`)

_[back to top](#language-tour)_

In Bella, variables (also known as _bindings_) are _immutable_; that is, they cannot be modified, reassigned, or changed in any way. The `let ... in` expression creates a variable for use in its body:

```bella
let my_name = "MystPi" in

Io.println(my_name)
```

Multiple variables can be defined at once:

```bella
let
  x = 1
  y = x + 1
  z = y * 2
  cool = true
in

[x, y, z, cool]
```

The body of a `let` expression comes after the `in` keyword. The body is a **single** expression:

```
let x = 5 in

; Prints `5`
Io.println(x)

; Error! There is no binding for `x`
Io.println(x)
```

> This can be made clearer with indentation:
>
> ```bella
> let x = 5 in
>   Io.println(x)
>
> ; Error! There is no binding for `x`
> Io.println(x)
> ```

> [As we will soon see](#expressions), parenthesis can be used to group multiple expressions into one.

## Lists

_[back to top](#language-tour)_

A list is a sequence of values, wrapped in square brackets.

```
["egg", "cheese", "lettuce", "tomato"]
```

Unlike some other programming languages, **any** combination of values can be in a list. They do not all have to be the same type.

```
[2, true, "foo", [x, y, z], 4.5]
```

Lists are immutable and cannot have items added or removed. The `+` operator helps with this by being able to concatenate two lists, returning a new one.

```
[1, 2, 3] + [4, 5, 6]
; => [1, 2, 3, 4, 5, 6]
```

> _This section is a work in progress_

## Records

_[back to top](#language-tour)_

A record is a collection of keys and values, also known as *fields*. The `.` operator is used to access a field.

```bella
let bella =
  { name: "Bella"
  , type: "dog"
  , living: false
  }
in

bella.name
; => "Bella"
```

Records, like everything in Bella, are immutable—a record's fields cannot be changed. However, the `+` operator can merge two records and return the combined one.

```bella
let
  first =
    { x: 1, y: 0 }

  second =
    { y: 2, z: 3}
in

first + second
; => { x: 1, y: 2, z: 3 }
```

> Tip: there is a helpful shorthand for writing record fields like `name: name` or `value: value`. In those cases, you can omit both the colon and the variable name:
>
> ```bella
> { name
> , value
> , foo: true
> }
> ```

## Lambdas

_[back to top](#language-tour)_

Lambdas are expressions that can be called with an argument and return a value.

```
x -> x * x
```

Declaring a function in Bella does not use special syntax. Instead, you assign a lambda expression to new variable.

```bella
let
  add_and_print =
    a ->
      Io.println(a + 100)
in

add_and_print(5)
```

Lambdas can have 0 or 1 parameters; no more, no less.

```
let
  calculate_magic_number =
    ; 0 parameters
    -> 42

  calculate_extra_magic_number =
    ; 1 parameter
    x ->
      x * calculate_magic_number()
in

calculate_extra_magic_number(25) ; => 1050
```

```
let
  add =
    a -> b -> a + b

  add_100 = add(100)
in

add_100(50) ; => 150
```

However, since lambda calls are automatically [_curried_](https://en.wikipedia.org/wiki/Currying), they have the appearance of being able to accept multiple arguments. The following two calls are equivalent:

```
do_something(a, b, c)
```

```
do_something(a)(b)(c)
```

The `|>` (pipe) operator is very useful for cleaning up convoluted calls like this one:

```bella
Io.println(List.first(parse(lex(read_file(file)))))
```

It takes the expression on the left and calls the lambda on the right with it. The example above can be rewritten with the pipe operator like so:

```bella
file
|> read_file
|> lex
|> parse
|> List.first
|> Io.println
```

Ah, much better!

> If you are confused, consider this smaller example:
>
> ```bella
> "Hello" |> Types.typeof |> Io.println
> ; that is the same as writing
> Io.println(Types.typeof("Hello"))
> ```

## Control Flow: `if` and `try`

_[back to top](#language-tour)_

There are only two control flow expressions: `if` and `try`. Loops, such as `while` or `for` in other languages, are accomplished via recursion in Bella.

### `if`

The `if` expression takes a Boolean condition and determines which branch to return depending on if the condition evaluates to `true`.

```bella
if 1 + 1 == 2 then
  "It's two!"
else
  "Math must be broken"

; => "It's two!"
```

Every `if` **must** be followed by an `else`. Multiple `if`-`else`s can be chained together:

```bella
if a then
  ; ...
else if b then
  ; ...
else if c then
  ; ...
else
  ; ...
```

### `try` (and `throw`)

The `try` expression handles runtime errors.

```bella
try
  ; Throws a 'cannot be called' error
  5()
else
  "There was an error!"
```

Like `if`, `try` must always be followed by an `else`. If the `else` expression is a lambda literal, it will be immediately called with the error message:

```bella
try
  1 + "2"
else msg ->
  Io.println(msg)

; Prints "Operands of + must be..."
```

An error can be manually thrown with `throw`.

```bella
throw "Oops!"
```

```bella
try
  throw "I am being thrown..."
else msg ->
  msg + " Now I'm caught!"

; => "I am being thrown... Now I'm caught!"
```

## Expressions

_[back to top](#language-tour)_

I have been using the term 'expression' quite a lot. But what _is_ an expression?

Quite simply, an expression is a bit of code that returns a value. `42`, `"hello"`, `5 * 6 + 3`, and `do_something(blah)` are all expressions. Even `if`, `try`, and `let` are expressions since they return values as well:

```bella
if true then 5 else 6
; => 5
```

```bella
Io.println(
  try
    throw "Pointless throw"
  else msg -> msg
)

; => "Pointless throw"
```

```
"Hello, " + (
  let
    your_name = Io.readline("What is your name?")
  in

  if your_name == "MystPi" then
    "weirdo"
  else
    your_name
) + "!"
```

Parenthesis can be used to group multiple expressions into one. The last expression inside the parenthesis gets returned.

```bella
(1 2 3)
; => 3
```

```bella
let x = 5 in
(
  x + 1 |> Io.println
  x - 2 |> Io.println
  x * 3 |> Io.println
)
```

They are also quite useful for grouping math operations:

```bella
1 + 2 * 3
; => 7

(1 + 2) * 3
; => 9
```
