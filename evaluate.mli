include module type of Types

val group_of_gm : gm -> group

val init_position : position
val init_regs_with_axis : regs_with_axis
val init : machine_state

val motion_of_commands : command list -> g_motion option

val evaluate_step : machine_state ->  word list -> step_result

val evaluate_gcode : ?machine_state : machine_state -> Lexing.lexbuf -> step_result BatEnum.t
