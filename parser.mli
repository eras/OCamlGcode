type axis = [ `X | `Y | `Z | `E | `A | `B | `C ]

type rest = string

type move = G0 | G1

module Axis : Map.OrderedType with type t = axis

module AxisMap : Map.S with type key = axis

type position = float AxisMap.t

type machine_state = {
  ms_coord_mode : [`Absolute | `Relative]
}

val default_machine_state : machine_state

type word =
    Move of (move * position * rest)
  | G90abs of rest
  | G91rel of rest
  | G92 of (position * rest)
  | Other of string

val string_of_gfloat : float -> string

val string_of_token : Lexer.token -> string

val parse_gcode : ?machine_state : machine_state -> Lexing.lexbuf -> word BatEnum.t

val string_of_input : ?machine_state : machine_state -> word -> (string * machine_state)
