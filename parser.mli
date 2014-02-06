type position = {
  x : float option;
  y : float option;
  z : float option;
  e : float option;
  rest : string;
}

type rest = string

type move = G0 | G1

type input =
    Move of (move * position)
  | G90abs of rest
  | G91rel of rest
  | G92 of position
  | Other of string

val string_of_gfloat : float -> string

val string_of_token : Lexer.token -> string

val parse_gcode : Lexing.lexbuf -> input BatEnum.t

val string_of_input :
  ?mode:[< `Absolute | `Relative > `Absolute ] ->
  ?previous:input -> input -> string
