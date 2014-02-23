type axis = [ `X | `Y | `Z | `E | `A | `B | `C ]

type rest = string

type move_reg = G0 | G1

type arc_reg = G2 | G3

module Axis : Map.OrderedType with type t = axis

module AxisMap : Map.S with type key = axis

type position = float AxisMap.t

type machine_state = {
  ms_coord_mode : [`Absolute | `Relative];
  ms_position	: position;
}

type arc_offset = {
  ao_i : float option;
  ao_j : float option;
}

type move = {
  move_reg  : move_reg;
  move_pos  : position;
  move_rest : rest;
}

type arc = {
  arc_reg    : arc_reg;
  arc_pos    : position;
  arc_offset : arc_offset;
  arc_rest   : rest;
}

type word =
| Move of move
| ArcCenter of arc
| G90abs of rest
| G91rel of rest
| G92 of (position * rest)
| Other of string

val default_machine_state : machine_state

val string_of_gfloat : float -> string

val string_of_token : Lexer.token -> string

val parse_gcode : ?machine_state : machine_state -> Lexing.lexbuf -> word BatEnum.t

val string_of_input : ?machine_state : machine_state -> word -> (string * machine_state)
