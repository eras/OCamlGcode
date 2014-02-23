open Batteries

module Lnoexn = BatList.Exceptionless

let ( **> ) a b = a b

type axis = [ `X | `Y | `Z | `E | `A | `B | `C ]

module Axis = struct type t = axis let compare = compare end

module AxisMap = Map.Make(struct type t = axis let compare = compare end)

type position = float AxisMap.t

type rest = string

type move_reg = G0 | G1

type arc_reg = G2 | G3

type machine_state = {
  ms_coord_mode : [`Absolute | `Relative];
  ms_position	: position;
}

let default_machine_state = {
  ms_coord_mode = `Absolute;
  ms_position   = AxisMap.empty;
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

let string_of_gfloat f =
  let str = Printf.sprintf "%.5f" f in
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
  | Some ofs when str.[ofs] = '.' -> String.sub str 0 (ofs + 2)
  | Some ofs -> String.sub str 0 (ofs + 1)
  )

let string_of_token = function
  | Lexer.Eof -> ""
  | Lexer.Entry (register, Lexer.Int value) -> Printf.sprintf "%c%d" register value
  | Lexer.Entry (register, Lexer.Float value) -> Printf.sprintf "%c%s" register (string_of_gfloat value)
  | Lexer.Comment str -> str
  | Lexer.Eol -> "\n"

let coalesce2 a b =
  match a with
    | None -> b
    | Some _ -> a

let app3 f (x, y, z) = (f x, f y, f z)

let app4 f (x, y, z, e) = (f x, f y, f z, f e)

let zip4 (a1, a2, a3, a4) (b1, b2, b3, b4) = ((a1, b1), (a2, b2), (a3, b3), (a4, b4))

let axis_symbols = [(`X, 'X'); (`Y, 'Y'); (`Z, 'Z'); (`E, 'E'); (`A, 'A'); (`B, 'B'); (`C, 'C')]

let axis, axis_chars = List.split axis_symbols

let register_symbols = axis_symbols @ [(`G, 'G'); (`M, 'M'); (`T, 'T'); (`I, 'I'); (`J, 'J')]

let move_g_values = [0; 1; 2; 3; 90; 91; 92] (* list of Gxx commands that affect device position *)

let registers, register_chars = List.split register_symbols

let char_of_register register = List.assoc register register_symbols

let char_of_axis axis = List.assoc axis axis_symbols

let parse_gcode ?machine_state lex_input =
  let next = ref None in
  let rec eof () =
    next := Some eof;
    raise BatEnum.No_more_elements;
  in
  let mode = ref `Absolute in
  let prev_at = ref AxisMap.empty in
  let prev_move_g = ref None in
  let process words =
    let get_float x = 
      match Lnoexn.find (function Lexer.Entry (reg, _) when reg = x -> true | _ -> false) words with
	| None -> None
	| Some (Lexer.Entry (_, Lexer.Float value)) -> Some value
	| Some (Lexer.Entry (_, Lexer.Int value)) -> Some (float_of_int value)
	| Some _ -> assert false
    in
    let at = List.filter_map (
      fun (symbol, char) ->
	match get_float char with
	| None -> None
	| Some x -> Some (symbol, x)
    ) axis_symbols in
    let has_op = 			(* has g or m-code *)
      List.exists (function
      | Lexer.Entry (('G' | 'M'), _) -> true
      | _ -> false
      ) words
    in
    let cur_move_g = 
      List.fold_left (
	fun cur_move_g x ->
	  match x with
	  | Lexer.Entry ('G', Lexer.Int n) -> Some n
	  | Lexer.Entry ('G', Lexer.Float f) when abs_float (fst (modf f)) < 0.0001 -> Some (truncate f)
	  | _ -> cur_move_g
      )
	None
	words
    in
    let cur_move_g =
      match cur_move_g, has_op with
      | Some move, _ -> cur_move_g
      | None, true   -> None
      | None, false  -> !prev_move_g
    in
    let i = get_float 'I' in
    let j = get_float 'J' in
    let rest = 
      lazy (
	let r = List.filter (function Lexer.Entry (reg, _) when List.mem reg register_chars -> false | _ -> true) words in
	  String.concat "" **> List.rev_map string_of_token r
      ) in
    let new_pos = 
      match !mode with
	| `Absolute -> 
	  List.fold_left (fun at (axis, value) -> AxisMap.add axis value at) !prev_at at
	| `Relative -> 
	  List.fold_left (
	    fun at (axis, value) -> 
	      let old_value = AxisMap.find axis !prev_at in
	      AxisMap.add axis (old_value +. value) at
	  ) !prev_at at
    in
    let update_positions () = prev_at := new_pos in
    let value =
      match cur_move_g with
      | Some 90 ->
	mode := `Absolute;
	G90abs (Lazy.force rest)
      | Some 91 ->
	mode := `Relative;
	G91rel (Lazy.force rest)
      | Some 0 ->
	update_positions ();
	(Move { move_reg = G0; move_pos = new_pos; move_rest = Lazy.force rest })
      | Some 1 ->
	update_positions ();
	(Move { move_reg = G1; move_pos = new_pos; move_rest = Lazy.force rest})
      | Some 2 when i <> None || j <> None ->
	update_positions ();
	(ArcCenter { arc_reg = G2; arc_pos = new_pos; arc_offset = { ao_i = i; ao_j = j; }; arc_rest = Lazy.force rest })
      | Some 3 when i <> None || j <> None ->
	update_positions ();
	(ArcCenter { arc_reg = G3; arc_pos = new_pos; arc_offset = { ao_i = i; ao_j = j; }; arc_rest = Lazy.force rest })
      | Some 92 ->
	update_positions ();
	G92 (new_pos, Lazy.force rest)
      | _ -> 
	Other (String.concat " " (List.rev_map string_of_token words))
    in
    prev_move_g := cur_move_g;
    value
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
		process accu
	    | Lexer.Entry _ as entry ->
		loop (entry::accu)
	    | Lexer.Comment _ as token ->
		next := Some (fun () -> Other (string_of_token token));
		process accu
	    | Lexer.Eol ->
		process (Lexer.Eol::accu)
  in
    BatEnum.from (fun () -> loop [])

let string_of_input ?(machine_state = default_machine_state) word = 
  (* let at' =  *)
  (*   match mode, previous with *)
  (*     | `Absolute, Some (Move ((G0 | G1), at, rest )) -> at *)
  (*     | `Absolute, Some (G92 (at, rest )) -> at *)
  (*     | (`Absolute | `Relative), Some (G90abs _ | G91rel _ | Other _)  *)
  (*     | (`Absolute | `Relative), None *)
  (*     | `Relative, _ -> AxisMap.empty *)
  (* in *)
  let string_of_coords at =
    let f label x x' = 
      match machine_state.ms_coord_mode with
      | `Absolute ->
	( match x', x with
	| _, None -> ""
	| Some x', Some x when string_of_gfloat x = string_of_gfloat x' -> ""
	| _, Some x -> Printf.sprintf " %s%s" label (string_of_gfloat x) )
      | `Relative ->
	match x with
	| Some x when string_of_gfloat x <> string_of_gfloat 0.0 -> Printf.sprintf " %s%s" label (string_of_gfloat x)
	| None | Some _ -> ""
    in
    String.concat "" (
      AxisMap.fold
	(fun axis value regs ->
	  f (String.make 1 (char_of_axis axis)) (Some value) None::regs
	)
	at
	[]
    )
  in
  let coord_cmd label at rest =
    (* this logic doesn't work. TODO: working elimination of redundant information *)
    (* label ^  *)
    (*   List.map  *)
    (*   (fun (axis, value) -> *)
    (* 	if AxisMap.mem )  *)
    (*   at *)
    (*   label ^ f "X" x_opt x_opt' ^ f "Y" y_opt y_opt' ^ f "Z" z_opt z_opt' ^ f "E" e_opt e_opt' ^ " " ^ rest *)
    label ^ string_of_coords at
  in
  let arc_cmd label at center rest =
    let string_of_opt_float key value =
      Option.default "" @@
	flip Option.map value @@ fun v ->
	  key ^ string_of_gfloat v
    in	
    label 
    ^ string_of_coords at
    ^ string_of_opt_float "I" center.ao_i 
    ^ string_of_opt_float "I" center.ao_j
    ^ " " ^ rest
  in
  let string_of_command = function
    | Move { move_reg = G0 } -> "G0"
    | Move { move_reg = G1 } -> "G1"
    | ArcCenter { arc_reg = G2 } -> "G2"
    | ArcCenter { arc_reg = G3 } -> "G3"
    | G92 _ -> "G92"
    | G90abs _ -> "G92"
    | G91rel _ -> "G92"
    | Other str -> failwith ("string_of_command not implemented for " ^ str)
  in
  let update_ms_coords machine_state at =
    match machine_state.ms_coord_mode with
    | `Absolute -> 
      let at = 
	AxisMap.fold
	  (fun axis value at ->
	    if AxisMap.mem axis at
	    then at
	    else AxisMap.add axis value at 
	  )
	  machine_state.ms_position
	  at
      in
      { machine_state with 
	ms_position = at
      }
    | `Relative ->
      let at = 
	AxisMap.fold
	  (fun axis value at ->
	    if not (AxisMap.mem axis at)
	    then (failwith "Cannot handle relative move without all current known axis")
	    else AxisMap.add axis (AxisMap.find axis at +. value) at 
	  )
	  machine_state.ms_position
	  at
      in
      { machine_state with 
	ms_position = at
      }
  in
  let str, machine_state =
    match word with
    | Move { move_reg = (G0 | G1); move_pos = at; move_rest = rest }-> 
      (coord_cmd (string_of_command word) at rest, update_ms_coords machine_state at)
    | ArcCenter { arc_reg = (G2 | G3); arc_pos = at; arc_offset = center; arc_rest = rest } -> 
      arc_cmd (string_of_command word) at center rest, update_ms_coords machine_state at
    | G92 (at, rest) -> (coord_cmd "G92" at rest, update_ms_coords machine_state at)
    | G90abs rest -> ("G90 " ^ rest, machine_state)
    | G91rel rest -> ("G91 " ^ rest, machine_state)
    | Other str -> (str, machine_state)
  in
  (str, machine_state)
