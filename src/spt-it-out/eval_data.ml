(** Evaluating data.

    @author eaburns
    @since 2010-06-19
*)

open Geometry
open Printf

let read_floats inch =
  (** [read_floats inch] reads the floats from the given channel. *)
  let floats = ref [] in
    try (while true do
	   let p = Scanf.fscanf inch " %f" (fun x -> x) in
	     floats := p :: !floats
	 done;
	 failwith "Impossible")
    with End_of_file ->
      `Scalars (Array.of_list (List.rev !floats))


let read_points inch =
  (** [read_points inch] reads the points from the given channel. *)
  let points = ref [] in
    try (while true do
	   let p = Scanf.fscanf inch " %f %f" (fun x y  -> point x y) in
	     points := p :: !points
	 done;
	 failwith "Impossible")
    with End_of_file ->
      `Points (Array.of_list (List.rev !points))


let read_triples inch =
  (** [read_triples inch] reads the triples from the given channel. *)
  let triples = ref [] in
    try (while true do
	   let t =
	     Scanf.fscanf inch " %f %f %f" (fun i j k  -> triple i j k)
	   in
	     triples := t :: !triples
	 done;
	 failwith "Impossible")
    with End_of_file ->
      `Triples (Array.of_list (List.rev !triples))


let eval_scalars eval_rec env line operands =
  (** [eval_scalars eval_rec env line operands] evaluates a scalar
      list. *)
  let lst =
    List.fold_right
      (fun p l -> match p with
	 | Sexpr.Number (_, x) ->  x :: l
	 | e ->
	     failwith (sprintf "line %d: Malformed scalar"
			 (Sexpr.line_number e)))
      operands []
  in `Scalars (Array.of_list lst)


let eval_scalars_file eval_rec env line = function
    (** [eval_scalars_file eval_rec env line operands] evaluates a
	scalar file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let floats = read_floats inch in
	close_in inch;
	floats
  | _ -> failwith (sprintf "line %d: Malformed scalar-file expression"
		     line)

let eval_scalars_cmd eval_rec env line = function
    (** [eval_scalars_cmd eval_rec env line operands] evaluates a
	scalar command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let floats = read_floats inch in
	ignore (Unix.close_process_in inch);
	floats
  | _ -> failwith (sprintf "line %d: Malformed scalar-cmd expression"
		     line)


let eval_points eval_rec env line operands =
  (** [eval_points eval_rec env line operands] evaluates a points
      list. *)
  let lst =
    List.fold_right
      (fun p l -> match p with
	 | Sexpr.List (_, Sexpr.Number (_, x)
			 :: Sexpr.Number(_, y) :: []) ->
	     (point x y) :: l
	 | e ->
	     failwith (sprintf "line %d: Malformed point"
			 (Sexpr.line_number e)))
      operands []
  in `Points (Array.of_list lst)


let eval_points_file eval_rec env line = function
    (** [eval_points_file eval_rec env line operands] evaluates a
	points file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let points = read_points inch in
	close_in inch;
	points
  | _ -> failwith (sprintf "line %d: Malformed points-file expression"
		     line)

let eval_points_cmd eval_rec env line = function
    (** [eval_points_cmd eval_rec env line operands] evaluates a
	points command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let points = read_points inch in
	ignore (Unix.close_process_in inch);
	points
  | _ -> failwith (sprintf "line %d: Malformed points-cmd expression"
		     line)


let eval_triples eval_rec env line operands =
  (** [eval_triples eval_rec env line operands] evaluates a triples
      list. *)
  let lst =
    List.fold_right
      (fun p l -> match p with
	 | Sexpr.List (_, Sexpr.Number (_, i)
			 :: Sexpr.Number (_, j)
			 :: Sexpr.Number (_, k) :: []) ->
	     (triple i j k) :: l
	 | e ->
	     failwith (sprintf "line %d: Malformed point"
			 (Sexpr.line_number e)))
      operands []
  in `Triples (Array.of_list lst)


let eval_triples_file eval_rec env line = function
    (** [eval_triples_file eval_rec env line operands] evaluates a
	triples file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let trips = read_triples inch in
	close_in inch;
	trips
  | _ -> failwith (sprintf "line %d: Malformed triples-file expression"
		     line)

let eval_triples_cmd eval_rec env line = function
    (** [eval_triples_cmd eval_rec env line operands] evaluates a
	triples command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let trips = read_triples inch in
	ignore (Unix.close_process_in inch);
	trips
  | _ -> failwith (sprintf "line %d: Malformed triples-cmd expression"
		     line)


let functions = [
  "scalars", eval_scalars;
  "scalars-file", eval_scalars_file;
  "scalars-cmd", eval_scalars_cmd;
  "points", eval_points;
  "points-file", eval_points_file;
  "points-cmd", eval_points_cmd;
  "triples", eval_triples;
  "triples-file", eval_triples_file;
  "triples-cmd", eval_triples_cmd;
]


let scalars eval_rec env exp = match eval_rec env exp with
  | `Scalars p -> p
  | x -> failwith (sprintf "line %d: Expected scalars" (Sexpr.line_number exp))


let points eval_rec env exp = match eval_rec env exp with
  | `Points p -> p
  | x -> failwith (sprintf "line %d: Expected points" (Sexpr.line_number exp))


let rec triples eval_rec env exp = match eval_rec env exp with
  | `Triples p -> p
  | x -> failwith (sprintf "line %d: Expected scalars" (Sexpr.line_number exp))
