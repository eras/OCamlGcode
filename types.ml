type coord_mode =
| CoordModeAbsolute
| CoordModeRelative

type axis = [ `X | `Y | `Z | `E | `A | `B | `C ]
let axis = [ `X; `Y; `Z; `E; `A; `B; `C ]

type reg_no_axis = [ `I | `J | `F | `T | `S | `K | `D | `L | `R | `N | `P | `Q ]
let regs_no_axis = [ `I; `J; `F; `T; `S; `K; `D; `L; `R; `N; `P; `Q ]

type reg_with_axis = [ axis | reg_no_axis ]
let regs_with_axis = axis @ regs_no_axis

type reg_all = [ reg_with_axis | `G | `M ]
let reg_all = regs_with_axis @ [ `G; `M ]

let reg_cmd_of_char : char -> reg_all = function
  | 'X' -> `X
  | 'Y' -> `Y
  | 'Z' -> `Z
  | 'E' -> `E
  | 'A' -> `A
  | 'B' -> `B
  | 'C' -> `C
  | 'I' -> `I
  | 'J' -> `J
  | 'F' -> `F
  | 'T' -> `T
  | 'S' -> `S
  | 'K' -> `K
  | 'D' -> `D
  | 'L' -> `L
  | 'R' -> `R
  | 'N' -> `N
  | 'P' -> `P
  | 'Q' -> `Q
  | 'G' -> `G
  | 'M' -> `M
  | ch -> failwith ("Unsupported register: " ^ String.make 1 ch)

let char_of_reg_cmd : reg_all -> char = function
  | `X -> 'X'
  | `Y -> 'Y'
  | `Z -> 'Z'
  | `E -> 'E'
  | `A -> 'A'
  | `B -> 'B'
  | `C -> 'C'
  | `I -> 'I'
  | `J -> 'J'
  | `F -> 'F'
  | `T -> 'T'
  | `S -> 'S'
  | `K -> 'K'
  | `D -> 'D'
  | `L -> 'L'
  | `R -> 'R'
  | `N -> 'N'
  | `P -> 'P'
  | `Q -> 'Q'
  | `G -> 'G'
  | `M -> 'M'

type rest = string

module Axis    = struct type t = axis let compare = compare end
module AxisMap = BatMap.Make(struct type t = axis let compare = compare end)
type position  = float AxisMap.t

module RegWithAxis     = struct type t = reg_with_axis let compare = compare end
module RegWithAxisMap  = BatMap.Make(struct type t = reg_with_axis let compare = compare end)
type regs_with_axis    = float RegWithAxisMap.t

module RegNoAxis     = struct type t = reg_no_axis let compare = compare end
module RegNoAxisMap  = BatMap.Make(struct type t = reg_no_axis let compare = compare end)
type regs_no_axis    = float RegNoAxisMap.t

type word = reg_all * float

(* group 0 = {G4, G10, G28, G30, G53, G92, G92.1, G92.2, G92.3} *)
type g_nonmodal                   = [`G4 | `G10 | `G28 | `G30 | `G53 | `G92 | `G92_1 | `G92_2 | `G92_3]

let axis_word_using_nonmodal      = [ `G10; `G28; `G30; `G92 ]

(* group 1 = {G0, G1, G2, G3, G38.2, G80, G81, G82, G83, G84, G85, G86, G87, G88, G89} motion *)
type g_motion                     = [`G0 | `G1 | `G2 | `G3 | `G38_2 | `G80 | `G81 | `G82 | `G83 | `G84 | `G85 | `G86 | `G87 | `G88 | `G89]
(* group 2 = {G17, G18, G19} plane selection *)
type g_plane                      = [`G17 | `G18 | `G19]
(* group 3 = {G90, G91} distance mode *)
type g_distance                   = [`G90 | `G91]
(* group 5 = {G93, G94} feed_rate mode *)
type g_feed_rate                  = [`G93 | `G94]
(* group 6 = {G20, G21} units *)
type g_units                      = [`G20 | `G21]
(* group 7 = {G40, G41, G42} cutter_radius_compensation *)
type g_cutter_radius_compensation = [`G40 | `G41 | `G42]
(* group 8 = {G43, G49} tool length_offset *)
type g_length_offset              = [`G43 | `G49]
(* group 10 = {G98, G99} return_mode in canned cycles *)
type g_return_mode                = [`G98 | `G99]
(* group 12 = {G54, G55, G56, G57, G58, G59, G59.1, G59.2, G59.3} coordinate_system selection *)
type g_coordinate_system          = [`G54 | `G55 | `G56 | `G57 | `G58 | `G59 | `G59_1 | `G59_2 | `G59_3]
(* group 13 = {G61, G61.1, G64} path_control mode *)
type g_path_control               = [`G61 | `G61_1 | `G64]

(* group 4 = {M0, M1, M2, M30, M60} stopping *)
type m_stopping                   = [`M0 | `M1 | `M2 | `M30 | `M60]
(* group 6 = {M6} tool change *)
type m_tool                       = [`M6]
(* group 7 = {M3, M4, M5} spindle turning *)
type m_spindle                    = [`M3 | `M4 | `M5]
(* group 8 = {M7, M8, M9} coolant (special case: M7 and M8 may be active at the same time) *)
type m_coolant                    = [`M7 | `M8 | `M9]
(* group 9 = {M48, M49} enable/disable feed and speed override switches *)
type m_override                   = [`M48 | `M49]

type group = [
| `NonModal                             (* group 0 *)
| `Motion                               (* group 1 *)
| `Plane                                (* group 2 *)
| `Distance                             (* group 3 *)
| `FeedRate                             (* group 5 *)
| `Units                                (* group 6 *)
| `CutterRadiusCompensation             (* group 7 *)
| `LengthOffset                         (* group 8 *)
| `ReturnMode                           (* group 10 *)
| `CoordinateSystem                     (* group 12 *)
| `PathControl                          (* group 13 *)
| `Stopping                             (* group 4 *)
| `Tool                                 (* group 6 *)
| `Spindle                              (* group 7 *)
| `Coolant                              (* group 8 *)
| `Override                             (* group 9 *)
]

type entry_words = {
  ew_g_nonmodal                   : g_nonmodal option;

  ew_g_motion                     : g_motion option;
  ew_g_plane                      : g_plane option;
  ew_g_distance                   : g_distance option;
  ew_g_feed_rate                  : g_feed_rate option;
  ew_g_units                      : g_units option;
  ew_g_cutter_radius_compensation : g_cutter_radius_compensation option;
  ew_g_length_offset              : g_length_offset option;
  ew_g_return_mode                : g_return_mode option;
  ew_g_coordinate_system          : g_coordinate_system option;
  ew_g_path_control               : g_path_control option;

  ew_m_stopping                   : m_stopping option;
  ew_m_tool                       : m_tool option;
  ew_m_spindle                    : m_spindle option;
  ew_m_coolant                    : m_coolant option;
  ew_m_override                   : m_override option;

  ew_axis                         : position;
  ew_regs                         : regs_no_axis;
}

type machine_state = {
  ms_position                     : position;
  ms_regs                         : regs_no_axis;

  ms_g_motion                     : g_motion;
  ms_g_plane                      : g_plane;
  ms_g_distance                   : g_distance;
  ms_g_feed_rate                  : g_feed_rate;
  ms_g_units                      : g_units;
  ms_g_cutter_radius_compensation : g_cutter_radius_compensation;
  ms_g_length_offset              : g_length_offset;
  ms_g_return_mode                : g_return_mode;
  ms_g_coordinate_system          : g_coordinate_system;
  ms_g_path_control               : g_path_control;

  (* ms_m_stopping                   : m_stopping; *)
  (* ms_m_tool                       : m_tool; *)
  (* ms_m_spindle                    : m_spindle; *)
  (* ms_m_coolant                    : m_coolant; *)
  (* ms_m_override                   : m_override; *)
}

type command = [
| g_nonmodal
| g_motion
(* | g_plane *)
(* | g_distance *)
(* | g_feed_rate *)
(* | g_units *)
(* | g_cutter_radius_compensation *)
(* | g_length_offset *)
(* | g_return_mode *)
(* | g_coordinate_system *)
(* | g_path_control *)
(* | m_stopping *)
(* | m_tool *)
(* | m_spindle *)
(* | m_coolant *)
(* | m_override *)
]

type gm = [
| g_nonmodal
| g_motion
| g_plane
| g_distance
| g_feed_rate
| g_units
| g_cutter_radius_compensation
| g_length_offset
| g_return_mode
| g_coordinate_system
| g_path_control
| m_stopping
| m_tool
| m_spindle
| m_coolant
| m_override
]

type step_result = {
  sr_state0   : machine_state;             (* machine state before evaluating this step *)
  sr_state1   : machine_state;             (* machine state after evaluating this step *)
  sr_regs     : regs_no_axis;              (* register values from this line; basically the words *)
  sr_commands : command list;              (* commands that were in effect when this line was evaluated *)
}
