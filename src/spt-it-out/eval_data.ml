(** Evaluating data.

    @author eaburns
    @since 2010-06-19
*)

open Geometry
open Printf
open Evaluate

let read_floats inch =
  (** [read_floats inch] reads the floats from the given channel. *)
  let floats = ref [] in
    try (while true do
	   let p = Scanf.fscanf inch " %f" (fun x -> x) in
	     floats := p :: !floats
	 done;
	 failwith "Impossible")
    with End_of_file ->
      Scalars (Array.of_list (List.rev !floats))


let read_points inch =
  (** [read_points inch] reads the points from the given channel. *)
  let points = ref [] in
    try (while true do
	   let p = Scanf.fscanf inch " %f %f" (fun x y  -> point x y) in
	     points := p :: !points
	 done;
	 failwith "Impossible")
    with End_of_file ->
      Points (Array.of_list (List.rev !points))


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
      Triples (Array.of_list (List.rev !triples))


(** {1 Convenience} ****************************************)

let eval_scalars eval_rec env line operands =
  (** [eval_scalars eval_rec env line operands] evaluates a scalar
      list. *)
  let lst =
    List.fold_right
      (fun p l -> match p with
	 | Sexpr.Number (_, x) ->  x :: l
	 | e ->
	     printf "line %d: Malformed scalar" (Sexpr.line_number e);
	     raise (Evaluate.Invalid_argument (Sexpr.line_number e)))
      operands []
  in Scalars (Array.of_list lst)

let help_str_scalars =
  "(scalars <number>+)\nCreates a list of scalar values"


let eval_scalars_file eval_rec env line = function
    (** [eval_scalars_file eval_rec env line operands] evaluates a
	scalar file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let floats = read_floats inch in
	close_in inch;
	floats
  | _ ->
      printf "line %d: Malformed scalar-file expression" line;
      raise (Evaluate.Invalid_argument line)


let help_str_scalars_file =
  "(scalars-file <string>)\n\
Opens the given file and reads in floating point numbers in ASCII\n\
format."



let eval_scalars_cmd eval_rec env line = function
    (** [eval_scalars_cmd eval_rec env line operands] evaluates a
	scalar command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let floats = read_floats inch in
	ignore (Unix.close_process_in inch);
	floats
  | _ ->
      printf "line %d: Malformed scalar-cmd expression" line;
      raise (Evaluate.Invalid_argument line)


let help_str_scalars_cmd =
  "(scalars-cmd <string>)\n\
Evaluates the given command-line using a shell.  The output\n\
of the command is read as a list of floating point numbers."

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
	     printf "line %d: Malformed point\n" (Sexpr.line_number e);
	     raise (Evaluate.Invalid_argument (Sexpr.line_number e)))
      operands []
  in Points (Array.of_list lst)


let help_str_points =
  "(points [(<number> <number>)]+)\nCreates a list of points."


let eval_points_file eval_rec env line = function
    (** [eval_points_file eval_rec env line operands] evaluates a
	points file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let points = read_points inch in
	close_in inch;
	points
  | _ ->
      printf "line %d: Malformed points-file expression" line;
      raise (Evaluate.Invalid_argument line)


let help_str_points_file =
  "(points-file <string>)\n\
Opens the given file and reads in floating point numbers in ASCII\n\
format.  It is assumed that there will be an even number of values\n\
in the file and each pair is taken to be a point x, y."


let eval_points_cmd eval_rec env line = function
    (** [eval_points_cmd eval_rec env line operands] evaluates a
	points command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let points = read_points inch in
	ignore (Unix.close_process_in inch);
	points
  | _ ->
      printf "line %d: Malformed points-cmd expression" line;
      raise (Evaluate.Invalid_argument line)


let help_str_points_cmd =
  "(points-cmd <string>)\n\
Evaluates the given command-line using a shell.  The output\n\
of the command is read as a list of floating point numbers.\n\
It is assumed that there will be an even number of values\n\
and each pair is taken to be a point x, y."


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
	     printf "line %d: Malformed point\n" (Sexpr.line_number e);
	     raise (Evaluate.Invalid_argument (Sexpr.line_number e)))
      operands []
  in Triples (Array.of_list lst)


let help_str_triples =
  "(triples [(<number> <number> <number>)]+)\nCreates a list of triples."



let eval_triples_file eval_rec env line = function
    (** [eval_triples_file eval_rec env line operands] evaluates a
	triples file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let trips = read_triples inch in
	close_in inch;
	trips
  | _ ->
      printf "line %d: Malformed triples-file expression\n" line;
      raise (Evaluate.Invalid_argument line)


let help_str_triples_file =
  "(triples-file <string>)\n\
Opens the given file and reads in floating point numbers in ASCII\n\
format.  It is assumed that there will be a number of values that\n\
is divisable by three.  Each set of three values is taken to be a\n\
triple i, j, k."


let eval_triples_cmd eval_rec env line = function
    (** [eval_triples_cmd eval_rec env line operands] evaluates a
	triples command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let trips = read_triples inch in
	ignore (Unix.close_process_in inch);
	trips
  | _ ->
      printf "line %d: Malformed triples-cmd expression\n" line;
      raise (Evaluate.Invalid_argument line)


let help_str_triples_cmd =
  "(points-cmd <string>)\n\
Evaluates the given command-line using a shell.  The output\n\
of the command is read as a list of floating point numbers.\n\
It is assumed that there will be a number of values that is \n\
divisable by three.  Each set of three values is taken to be a\n\
triple i, j, k."


let functions = [
  "scalars", eval_scalars, help_str_scalars;
  "scalars-file", eval_scalars_file, help_str_scalars_file;
  "scalars-cmd", eval_scalars_cmd, help_str_scalars_cmd;
  "points", eval_points, help_str_points;
  "points-file", eval_points_file, help_str_points_file;
  "points-cmd", eval_points_cmd, help_str_points_cmd;
  "triples", eval_triples, help_str_triples;
  "triples-file", eval_triples_file, help_str_triples_file;
  "triples-cmd", eval_triples_cmd, help_str_triples_cmd;
]

(** {1 Convenience} ****************************************)

let scalars eval_rec env exp = match eval_rec env exp with
  | Scalars p -> p
  | x -> failwith (sprintf "line %d: Expected scalars" (Sexpr.line_number exp))


let points eval_rec env exp = match eval_rec env exp with
  | Points p -> p
  | x -> failwith (sprintf "line %d: Expected points" (Sexpr.line_number exp))


let rec triples eval_rec env exp = match eval_rec env exp with
  | Triples p -> p
  | x -> failwith (sprintf "line %d: Expected scalars" (Sexpr.line_number exp))
