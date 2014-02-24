include module type of Types

val group_of_gm : gm -> group

val init_position : position
val init_regs : regs
val init : machine_state

val evaluate_step : machine_state ->  word list -> step_result

val evaluate_gcode : ?machine_state : machine_state -> Lexing.lexbuf -> step_result BatEnum.t
