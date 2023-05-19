import gleam/io
import gleam/result.{then as try}
import tokenizer
import parser
import evaluator

pub fn main() {
  let test_str =
    "
    let twelve =
      add5(7)
    in

    print(-length_of_vec(5)(6))
    "

  let test_str = "6 >= hi and true and { 1 or 2 }(1) 1"

  let test_str = "print, { length_of_vec, 5, 6 }"

  let test_str = "6 |> length_of_vec, 5 |> print"

  let test_str = "let x = 1 in { 1 + x } * 3"

  let test_str =
    "
    let
      x = 1
    in

    let
      y = 2
    in

    x + y
  "

  let test_str =
    "
    let
      add_x_y_z =
        x -> y -> z ->
          x + y + z
    in

    ; print,{add_x_y_z, 1, 2, 3}

    3 |> {add_x_y_z, 1, 2} |> print
  "

  let test_str =
    "
    let
      call =
        x -> arg ->
          x, arg
    in

    call, print, 'Hello, world!'
  "

  let test_str =
    "
; Let is an expression with TWO operands:
;   - its value
;   - the expression where it's used
; Define functions with lambdas
; Lambdas are always curried

let add =
  a -> b -> a + b
in

let add5 =
  add, 5
in

let twelve =
  add5, 7
in

print, twelve

let countdown =
  n ->
    if (n == 0)
      print, 'Liftoff!'
    else {
      print, n
      countdown,{ n - 1 }
    }
in

countdown, 10

let length_of_vec =
  x -> y ->
    let
      x_sqr = x * x
    in

    let
      y_sqr = y * y
    in

    x_sqr + y_sqr
in

print,{ length_of_vec, 5, 6 }

;let divide =
;  a -> b ->
;    if (b == 0)
;      throw 'Divisor cannot be 0'
;    else
;      a / b
;
;let
;  x = try divide(1, 0) else msg -> 0
;in

; x
  "

  let test_str = "
  let a = 'first' in
  let showA = n -> a in
  let a = 'second' in
  showA, 0 |> print
  "

  let test_str = "
  let fact =
    n ->
      if (n == 0)
        1
      else
        n * fact(n - 1)
  in

  4 |> fact |> print
  "

  let tokens = tokenizer.tokenize(test_str)
  io.debug(tokens)

  use tokens <- try(tokens)
  let parsed = parser.parse(tokens)
  io.debug(parsed)

  use parsed <- try(parsed)
  let result = evaluator.evaluate(parsed)
  io.debug(result)
}
