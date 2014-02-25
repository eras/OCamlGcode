include Types

let group_of_gm : gm -> group = function
|  `G4 | `G10 | `G28 | `G30 |   `G53 | `G92 | `G92_1 | `G92_2 | `G92_3 -> `NonModal
|  `G0 |  `G1 |  `G2 |  `G3 | `G38_2 | `G80 |   `G81 |   `G82 | `G83 | `G84 | `G85 | `G86 | `G87 | `G88 | `G89 -> `Motion
| `G17 | `G18 | `G19 | `Gnoplane -> `Plane
| `G90 | `G91 | `Gnodistance -> `Distance
| `G93 | `G94 -> `FeedRate
| `G20 | `G21 | `Gnounits -> `Units
| `G40 | `G41 | `G42 -> `CutterRadiusCompensation
| `G43 | `G49 -> `LengthOffset
| `G98 | `G99 -> `ReturnMode
| `G54 | `G55   | `G56 | `G57 | `G58 | `G59 | `G59_1 | `G59_2 | `G59_3 -> `CoordinateSystem
| `G61 | `G61_1 | `G64 -> `PathControl
|  `M0 |  `M1   | `M2 | `M30 | `M60 -> `Stopping
|  `M6 | `Mnotool -> `Tool
|  `M3 |  `M4 | `M5 -> `Spindle
|  `M7 |  `M8 | `M9 -> `Coolant
| `M48 | `M49 -> `Override

let gm_of_reg_value : ([ `G | `M ] * float) -> gm = fun (reg, value) ->
  match reg, truncate (value *. 1000.0 +. 0.5) with
  | `G,  0000  -> `G0
  | `G,  1000  -> `G1
  | `G,  2000  -> `G2
  | `G,  3000  -> `G3
  | `G,  4000  -> `G4
  | `G, 10000 -> `G10
  | `G, 17000 -> `G17
  | `G, 18000 -> `G18
  | `G, 19000 -> `G19
  | `G, 20000 -> `G20
  | `G, 21000 -> `G21
  | `G, 28000 -> `G28
  | `G, 30000 -> `G30
  | `G, 38200 -> `G38_2
  | `G, 40000 -> `G40
  | `G, 41000 -> `G41
  | `G, 42000 -> `G42
  | `G, 43000 -> `G43
  | `G, 49000 -> `G49
  | `G, 53000 -> `G53
  | `G, 54000 -> `G54
  | `G, 55000 -> `G55
  | `G, 56000 -> `G56
  | `G, 57000 -> `G57
  | `G, 58000 -> `G58
  | `G, 59000 -> `G59
  | `G, 59100 -> `G59_1
  | `G, 59200 -> `G59_2
  | `G, 59300 -> `G59_3
  | `G, 61000 -> `G61
  | `G, 61100 -> `G61_1
  | `G, 64000 -> `G64
  | `G, 80000 -> `G80
  | `G, 81000 -> `G81
  | `G, 82000 -> `G82
  | `G, 83000 -> `G83
  | `G, 84000 -> `G84
  | `G, 85000 -> `G85
  | `G, 86000 -> `G86
  | `G, 87000 -> `G87
  | `G, 88000 -> `G88
  | `G, 89000 -> `G89
  | `G, 90000 -> `G90
  | `G, 91000 -> `G91
  | `G, 92000 -> `G92
  | `G, 92100 -> `G92_1
  | `G, 92200 -> `G92_2
  | `G, 92300 -> `G92_3
  | `G, 93000 -> `G93
  | `G, 94000 -> `G94
  | `G, 98000 -> `G98
  | `G, 99000 -> `G99
  | `M,  0000  -> `M0
  | `M,  1000  -> `M1
  | `M,  2000  -> `M2
  | `M,  6000  -> `M6
  | `M,  3000  -> `M3
  | `M,  4000  -> `M4
  | `M,  5000  -> `M5
  | `M,  7000  -> `M7
  | `M,  8000  -> `M8
  | `M,  9000  -> `M9
  | `M, 30000 -> `M30
  | `M, 48000 -> `M48
  | `M, 49000 -> `M49
  | `M, 60000 -> `M60
  | _ -> failwith ("unknwon command")

let reg_value_of_gm : gm -> ([ `G | `M ] * float) = function
  | `G0 -> `G,  0.000
  | `G1 -> `G,  1.000
  | `G2 -> `G,  2.000
  | `G3 -> `G,  3.000
  | `G4 -> `G,  4.000
  | `G10 -> `G, 10.000
  | `G17 -> `G, 17.000
  | `G18 -> `G, 18.000
  | `G19 -> `G, 19.000
  | `Gnoplane -> assert false
  | `G20 -> `G, 20.000
  | `G21 -> `G, 21.000
  | `Gnounits -> assert false
  | `G28 -> `G, 28.000
  | `G30 -> `G, 30.000
  | `G38_2 -> `G, 38.200
  | `G40 -> `G, 40.000
  | `G41 -> `G, 41.000
  | `G42 -> `G, 42.000
  | `G43 -> `G, 43.000
  | `G49 -> `G, 49.000
  | `G53 -> `G, 53.000
  | `G54 -> `G, 54.000
  | `G55 -> `G, 55.000
  | `G56 -> `G, 56.000
  | `G57 -> `G, 57.000
  | `G58 -> `G, 58.000
  | `G59 -> `G, 59.000
  | `G59_1 -> `G, 59.100
  | `G59_2 -> `G, 59.200
  | `G59_3 -> `G, 59.300
  | `G61 -> `G, 61.000
  | `G61_1 -> `G, 61.100
  | `G64 -> `G, 64.000
  | `G80 -> `G, 80.000
  | `G81 -> `G, 81.000
  | `G82 -> `G, 82.000
  | `G83 -> `G, 83.000
  | `G84 -> `G, 84.000
  | `G85 -> `G, 85.000
  | `G86 -> `G, 86.000
  | `G87 -> `G, 87.000
  | `G88 -> `G, 88.000
  | `G89 -> `G, 89.000
  | `G90 -> `G, 90.000
  | `G91 -> `G, 91.000
  | `Gnodistance -> assert false
  | `G92 -> `G, 92.000
  | `G92_1 -> `G, 92.100
  | `G92_2 -> `G, 92.200
  | `G92_3 -> `G, 92.300
  | `G93 -> `G, 93.000
  | `G94 -> `G, 94.000
  | `G98 -> `G, 98.000
  | `G99 -> `G, 99.000
  | `M0 -> `M,  0.000
  | `M1 -> `M,  1.000
  | `M2 -> `M,  2.000
  | `M6 -> `M,  6.000
  | `Mnotool -> assert false
  | `M3 -> `M,  3.000
  | `M4 -> `M,  4.000
  | `M5 -> `M,  5.000
  | `M7 -> `M,  7.000
  | `M8 -> `M,  8.000
  | `M9 -> `M,  9.000
  | `M30 -> `M, 30.000
  | `M48 -> `M, 48.000
  | `M49 -> `M, 49.000
  | `M60 -> `M, 60.000

let string_of_gfloat f =
  let str = Printf.sprintf "%.8f" f in
  let last_non_zero =
    let rec find_last_nonzero ofs = 
      if ofs < 0
      then None
      else if str.[ofs] <> '0'
      then Some ofs
      else find_last_nonzero (ofs - 1)
    in
    find_last_nonzero (String.length str - 1)
  in
  ( match last_non_zero with
  | None -> str
  | Some ofs when str.[ofs] = '.' -> String.sub str 0 ofs
  | Some ofs -> String.sub str 0 (ofs + 1)
  )

let init_position =
  List.fold_left
    (fun axisMap axis -> AxisMap.add axis 0.0 axisMap)
    AxisMap.empty
    axis

let init_regs_with_axis =
  List.fold_left
    (fun regMap init -> RegWithAxisMap.add init 0.0 regMap)
    RegWithAxisMap.empty
    regs_with_axis

let init_regs_no_axis =
  List.fold_left
    (fun regMap init -> RegNoAxisMap.add init 0.0 regMap)
    RegNoAxisMap.empty
    regs_no_axis

let init_entry_words = {
  ew_g_nonmodal                   = None;

  ew_g_motion                     = None;
  ew_g_plane                      = None;
  ew_g_distance                   = None;
  ew_g_feed_rate                  = None;
  ew_g_units                      = None;
  ew_g_cutter_radius_compensation = None;
  ew_g_length_offset              = None;
  ew_g_return_mode                = None;
  ew_g_coordinate_system          = None;
  ew_g_path_control               = None;

  ew_m_stopping                   = None;
  ew_m_tool                       = None;
  ew_m_spindle                    = None;
  ew_m_coolant                    = None;
  ew_m_override                   = None;

  ew_axis                     = AxisMap.empty;
  ew_regs                         = RegNoAxisMap.empty;
}

let init = {
  ms_position                     = init_position;
  ms_regs                          = init_regs_no_axis;
  (* ms_feedrate                     = None; *)

  ms_g_motion                     = `G0;
  ms_g_plane                      = `Gnoplane;
  ms_g_distance                   = `Gnodistance;
  ms_g_feed_rate                  = `G94;
  ms_g_units                      = `Gnounits;
  ms_g_cutter_radius_compensation = `G40;
  ms_g_length_offset              = `G49;
  ms_g_return_mode                = `G99;
  ms_g_coordinate_system          = `G54;
  ms_g_path_control               = `G61;
  (* ms_m_stopping                   = m_stopping; *)
  ms_m_tool                       = `Mnotool;
  (* ms_m_spindle                    = m_spindle; *)
  (* ms_m_coolant                    = m_coolant; *)
  (* ms_m_override                   = m_override; *)
}

(* repeated entries within the group are forbidden, as in the G-code specs *)
let entry_words_of_words (words : word list) =
  let check x y =
    match x with
    | None -> y
    | Some _ -> failwith "entry_words_of_words: duplicate command value within a group"
  in
  List.fold_left 
    (fun ews ((reg, value) : word) ->
      match reg with
      | (`G | `M) as reg ->
        ( match gm_of_reg_value (reg, value) with
        | #g_nonmodal                   as cmd -> check ews.ew_g_nonmodal                   { ews with ew_g_nonmodal                   = Some cmd }
        | #g_motion                     as cmd -> check ews.ew_g_motion                     { ews with ew_g_motion                     = Some cmd }
        | #g_plane                      as cmd -> check ews.ew_g_plane                      { ews with ew_g_plane                      = Some cmd }
        | #g_distance                   as cmd -> check ews.ew_g_distance                   { ews with ew_g_distance                   = Some cmd }
        | #g_feed_rate                  as cmd -> check ews.ew_g_feed_rate                  { ews with ew_g_feed_rate                  = Some cmd }
        | #g_units                      as cmd -> check ews.ew_g_units                      { ews with ew_g_units                      = Some cmd }
        | #g_cutter_radius_compensation as cmd -> check ews.ew_g_cutter_radius_compensation { ews with ew_g_cutter_radius_compensation = Some cmd }
        | #g_length_offset              as cmd -> check ews.ew_g_length_offset              { ews with ew_g_length_offset              = Some cmd }
        | #g_return_mode                as cmd -> check ews.ew_g_return_mode                { ews with ew_g_return_mode                = Some cmd }
        | #g_coordinate_system          as cmd -> check ews.ew_g_coordinate_system          { ews with ew_g_coordinate_system          = Some cmd }
        | #g_path_control               as cmd -> check ews.ew_g_path_control               { ews with ew_g_path_control               = Some cmd }
        | #m_stopping                   as cmd -> check ews.ew_m_stopping                   { ews with ew_m_stopping                   = Some cmd }
        | #m_tool                       as cmd -> check ews.ew_m_tool                       { ews with ew_m_tool                       = Some cmd }
        | #m_spindle                    as cmd -> check ews.ew_m_spindle                    { ews with ew_m_spindle                    = Some cmd }
        | #m_coolant                    as cmd -> check ews.ew_m_coolant                    { ews with ew_m_coolant                    = Some cmd }
        | #m_override                   as cmd -> check ews.ew_m_override                   { ews with ew_m_override                   = Some cmd }
        )
      | #axis as axis -> { ews with ew_axis = AxisMap.add axis value ews.ew_axis }
      | #reg_no_axis as reg -> { ews with ew_regs = RegNoAxisMap.add reg value ews.ew_regs }
    )
    init_entry_words
    words

let motion_of_commands : command list -> g_motion option = fun commands ->
  let motions = BatList.filter_map (function #g_motion as m -> Some m | _ -> None) commands in
  match motions with
  | [] -> None
  | [motion] -> Some motion
  | _ -> failwith "Too many motions in one command list"

module FilterMap (S : Map.S) (T : Map.S) = struct
  let filter_map f m =
    S.fold (
      fun key value result ->
        match f key value with
        | None -> result
        | Some (key, value) -> T.add key value result
    ) m T.empty
end

module ListOfMap (S : Map.S) = struct
  let list_of_map m =
    List.rev (S.fold (fun key value result -> (key, value)::result) m [])
end

let replace_regs regs0 regs1 =
  RegNoAxisMap.fold
    RegNoAxisMap.add
    regs1
    regs0

let add_axis_absolute axis0 axis1 =
  AxisMap.fold
    AxisMap.add
    axis1
    axis0

let add_axis_relative axis0 axis1 =
  AxisMap.fold
    (fun axis value result ->
      AxisMap.add axis (AxisMap.find axis result +. value) result
    )
    axis1
    axis0

let string_of_word (reg_all, value) = Printf.sprintf "%c%s" (char_of_reg_cmd reg_all) (string_of_gfloat value)

let string_of_word_list word_list = List.map string_of_word word_list |> String.concat " "

let evaluate_step : machine_state -> word list -> step_result =
  fun state words ->
    let default = BatOption.default in
    let ews = entry_words_of_words words in
    let axis_word_using_nonmodal = 
      match ews.ew_g_nonmodal with
      | None -> None
      | Some x when List.mem x axis_word_using_nonmodal -> Some x
      | Some _ -> None
    in
    let commands =
      match axis_word_using_nonmodal,
            ews.ew_g_motion, 
            state.ms_g_motion with
      | Some nonmodal, None, _ -> [(nonmodal :> command)]
      | None, Some motion, _ when not (AxisMap.is_empty ews.ew_axis) -> [(motion :> command)]
      | None, Some motion, _                                  -> []
      | None, None, motion when not (AxisMap.is_empty ews.ew_axis) -> [(motion :> command)]
      | None, None, _                                       -> []
      | _ -> failwith ("Colliding axis-using nonmodal and modal commands: " ^ string_of_word_list words)
    in
    let commands =
      commands @
      match ews.ew_m_tool with
      | Some (#m_tool as m) -> [ m ]
      | None -> []
    in
    let position =
      let movement =
        match default state.ms_g_distance ews.ew_g_distance with
        | `G90 -> add_axis_absolute
        | `G91 -> add_axis_relative
        | `Gnodistance -> add_axis_absolute
      in
      movement state.ms_position ews.ew_axis
    in
    let state' = {
      ms_position                     = position;
      ms_regs                         = replace_regs state.ms_regs ews.ew_regs;

      ms_g_motion                     = default state.ms_g_motion                     ews.ew_g_motion;
      ms_g_plane                      = default state.ms_g_plane                      ews.ew_g_plane;
      ms_g_distance                   = default state.ms_g_distance                   ews.ew_g_distance;
      ms_g_feed_rate                  = default state.ms_g_feed_rate                  ews.ew_g_feed_rate;
      ms_g_units                      = default state.ms_g_units                      ews.ew_g_units;
      ms_g_cutter_radius_compensation = default state.ms_g_cutter_radius_compensation ews.ew_g_cutter_radius_compensation;
      ms_g_length_offset              = default state.ms_g_length_offset              ews.ew_g_length_offset;
      ms_g_return_mode                = default state.ms_g_return_mode                ews.ew_g_return_mode;
      ms_g_coordinate_system          = default state.ms_g_coordinate_system          ews.ew_g_coordinate_system;
      ms_g_path_control               = default state.ms_g_path_control               ews.ew_g_path_control;

      ms_m_tool                       = default `Mnotool                              ews.ew_m_tool;
    } in
    { sr_state0   = state;
      sr_state1   = state';
      sr_regs     = ews.ew_regs;
      sr_commands = commands }

let word_of_entry (char, value) =
  (reg_cmd_of_char char,
   match value with
   | Lexer.Int x -> float_of_int x
   | Lexer.Float x -> x)

let evaluate_gcode : ?machine_state : machine_state -> Lexing.lexbuf -> step_result BatEnum.t = 
  fun ?(machine_state=init) lex_input->
    let machine_state = ref machine_state in
    let next = ref None in
    let rec eof () =
      next := Some eof;
      raise BatEnum.No_more_elements;
    in
    let rec loop accu =
      match !next with
      | Some fn ->
	next := None;
	fn ()
      | None ->
	match Lexer.token lex_input with
	| Lexer.Eof -> 
	  next := Some eof;
	  let sr = evaluate_step !machine_state (List.rev accu) in
          machine_state := sr.sr_state1;
          sr
	| Lexer.Entry entry ->
	  loop (word_of_entry entry::accu)
	| Lexer.Comment _ ->
	  loop accu
	| Lexer.Eol ->
	  let sr = evaluate_step !machine_state (List.rev accu) in
          machine_state := sr.sr_state1;
          sr
    in
    BatEnum.from (fun () -> loop [])

let ms_g_motion x = x.ms_g_motion
let ms_g_plane x = x.ms_g_plane
let ms_g_distance x = x.ms_g_distance
let ms_g_feed_rate x = x.ms_g_feed_rate
let ms_g_units x = x.ms_g_units
let ms_g_cutter_radius_compensation x = x.ms_g_cutter_radius_compensation
let ms_g_length_offset x = x.ms_g_length_offset
let ms_g_return_mode x = x.ms_g_return_mode
let ms_g_coordinate_system x = x.ms_g_coordinate_system
let ms_g_path_control x = x.ms_g_path_control

let word_list_of_step_result : step_result -> word list =
  fun sr ->
    let a = sr.sr_state0 in
    let b = sr.sr_state1 in
    let cast_rv (reg, value) =
      ((reg :> reg_all), value)
    in
    let g f = 
      if ((f a) :> gm) <> ((f b) :> gm) then
        [cast_rv (reg_value_of_gm ((f b) :> gm))]
      else
        []
    in
    let words = [] in
    let filter_different a b =
      BatList.filter_map BatPervasives.identity (BatList.map2 (fun a b -> if a <> b then Some b else None) a b)
    in
    let words =
      assert (List.mem a.ms_g_distance [`G90; `Gnodistance]);  (* relative distances not supported yet *)
      let module LOM = ListOfMap(AxisMap) in
      let a' = LOM.list_of_map a.ms_position in
      let b' = LOM.list_of_map b.ms_position in
      words @ List.map cast_rv (filter_different a' b')
    in
    let words =
      let module LOM = ListOfMap(RegNoAxisMap) in
      let a' = LOM.list_of_map a.ms_regs in
      let b' = LOM.list_of_map b.ms_regs in
      words @ List.map cast_rv (filter_different a' b')
    in
    let words =
      words @
        List.map
        (fun x -> cast_rv (reg_value_of_gm (x :> gm)))
        sr.sr_commands
    in
    let words =
      g ms_g_plane @
        g ms_g_distance @
        g ms_g_feed_rate @
        g ms_g_units @
        g ms_g_cutter_radius_compensation @
        g ms_g_length_offset @
        g ms_g_return_mode @
        g ms_g_coordinate_system @
        g ms_g_path_control @
        words
    in 
    words

let string_of_step_result : step_result -> string =
  fun sr -> string_of_word_list (word_list_of_step_result sr)
