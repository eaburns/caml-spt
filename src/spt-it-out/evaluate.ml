(**

    @author eaburns
    @since 2010-06-19
*)

open Geometry
open Printf

type value = [
  | `Unit
  | `Number of float
  | `String of string
  | `Scalars of float array
  | `Points of point array
  | `Triples of triple array
  | `Num_by_num_dataset of Num_by_num.dataset_type
  | `Num_by_num_plot of Num_by_num.plot_type
  | `Num_by_nom_dataset of Num_by_nom.dataset_type
  | `Num_by_nom_plot of Num_by_nom.plot_type
]

type environment = {
  bindings : (string * value) list;
  next_glyph : unit -> Drawing.glyph;
  next_dash : unit -> Length.t array;
  next_line_errbar : unit -> Num_by_num.line_errbar_style;
  next_fill : unit -> Drawing.fill_pattern;
}


let init_environment =
  let next_dash = Factories.default_dash_factory () in
    {
      bindings = [];
      next_glyph = Factories.default_glyph_factory ();
      next_dash = next_dash;
      next_line_errbar = Num_by_num.line_errbar_factory next_dash ();
      next_fill = Factories.default_fill_pattern_factory ();
    }


let lookup_ident env l id =
  try List.assoc id env.bindings
  with Not_found -> failwith (sprintf "line %d: Unknown identifier: %s" l id)


let value_to_string = function
    (** [to_string t] gets the string representation. *)
  | `Unit -> "Unit"
  | `Number _ -> "Number"
  | `String _ -> "String"
  | `Scalars _ -> "Scalars"
  | `Points _ -> "Points"
  | `Triples _ -> "Triples"
  | `Num_by_num_dataset _ -> "Num_by_num_dataset"
  | `Num_by_num_plot _ -> "Num_by_num_plot"
  | `Num_by_nom_dataset _ -> "Num_by_nom_dataset"
  | `Num_by_nom_plot _ -> "Num_by_nom_plot"


let evaluate functs eval_rec env = function
    (** [evaluate functs eval_rec env expr] evaluates [expr].
	[functs] is a list of function names followed by their
	evaluation function.  [eval_rec] is the evaluation function to
	call when recursive evaluation is required. *)
  | Sexpr.List (_, (Sexpr.Ident (line, func_name)) :: operands ) ->
      begin try
	let f = List.assoc func_name functs in
	  f eval_rec env line operands
      with Not_found ->
	Printf.printf "Available functions:\n";
	List.iter (fun (n, _) -> Printf.printf "\t%s\n" n) functs;
	failwith (sprintf "line %d: Unknown function %s" line func_name)
      end
  | Sexpr.Number (_, vl) -> `Number vl
  | Sexpr.String (_, n) -> `String n
  | Sexpr.Ident (l, id) -> lookup_ident env l id
  | Sexpr.List (l, _) -> failwith (sprintf "line %d: Unexpected list" l)


(** {1 Default functions} *****************************)

let eval_let eval_rec env line = function
    (** [eval_let eval_rec env line operands] evaluates the operands
	of a let expression. *)
  | Sexpr.List (_, bindings) :: (Sexpr.List (_, _) as expr) :: [] ->
      let binds =
	List.fold_right
	  (fun b l ->
	     match b with
	       | Sexpr.List (_, Sexpr.Ident (_, name) :: value :: []) ->
		   (name, eval_rec env value) :: l
	       | e ->
		   failwith (sprintf "line %d: Malformed binding"
			       (Sexpr.line_number e)))
	  bindings []
      in
	eval_rec {env with bindings = (binds @ env.bindings)} expr
  | e :: _ ->
      failwith (sprintf "line %d: Malformed let statement" line)
  | [] ->
      failwith (sprintf "line %d: let expression: Unexpected end of file" line)


let eval_print eval_rec env line operands =
  (** [eval_print eval_rec env line operands] evaluates a print
      expression. *)
  List.fold_left
    (fun _ op ->
       let vl = eval_rec env op in
	 begin match vl with
	   | `Unit -> printf "()"
	   | `Number vl -> printf "%f" vl
	   | `String s -> printf "%s" s
	   | `Scalars s ->
	       printf "[| "; Array.iter (printf "%f; ") s; printf " |]"
	   | `Points s ->
	       printf "[| ";
	       Array.iter (fun p -> printf "(%f, %f); " p.x p.y) s;
	       printf "|]"
	   | `Triples s ->
	       printf "[| ";
	       Array.iter (fun t -> printf "(%f, %f, %f); " t.i t.j t.k) s;
	       printf "|]"
	   | `Num_by_num_dataset _ -> printf "<num-by-num-dataset>"
	   | `Num_by_num_plot _ -> printf "<num-by-num-plot>"
	   | `Num_by_nom_dataset _ -> printf "<num-by-nom-dataset>"
	   | `Num_by_nom_plot _ -> printf "<num-by-nom-plot>"
	 end;
	 vl)
    `Unit operands


let functions = [
  "let", eval_let;
  "print", eval_print;
]
