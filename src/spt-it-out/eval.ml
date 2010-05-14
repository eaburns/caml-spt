(** Analyze s-experssions, conversion to plotting language and execution.

    @author eaburns
    @since 2010-05-14
*)

open Printf
open Geometry

type t =
  | Unit
  | Number of float
  | String of string
  | Scalars of float array
  | Points of point array
  | Triples of triple array
  | Num_by_num_dataset of Num_by_num.dataset
  | Num_by_num_plot of Num_by_num.plot


type env = {
  bindings : (string * t) list;
  next_glyph : unit -> Drawing.glyph;
  next_dash : unit -> Length.t array
}


let init_env = {
  bindings = [];
  next_glyph = Scatter_dataset.default_glyph_factory ();
  next_dash = Line_dataset.default_dash_factory ();
}


let to_string = function
    (** [to_string t] gets the string representation. *)
  | Unit -> "Unit"
  | Number _ -> "Number"
  | String _ -> "String"
  | Scalars _ -> "Scalars"
  | Points _ -> "Points"
  | Triples _ -> "Triples"
  | Num_by_num_dataset _ -> "Num_by_num_dataset"
  | Num_by_num_plot _ -> "Num_by_num_plot"


let set_once reference line name vl =
  (** [set_once reference line name vl] sets the reference if it is
      not already set.  Otherwise an error is reported. *)
  if !reference = None
  then reference := Some vl
  else failwith (sprintf "line %d: %s already specified" line name)


let rec eval env = function
    (** [eval env e] evaluates the given s-expression. *)
  | Sexpr.List (_, (Sexpr.Ident (line, "let")) :: operands ) ->
      eval_let env line operands
  | Sexpr.List (_, (Sexpr.Ident (_, "print")) :: operands ) ->
      eval_print env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "points")) :: operands ) ->
      eval_points env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "triples")) :: operands ) ->
      eval_triples env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "scatter-dataset")) :: operands ) ->
      eval_scatter env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "line-dataset")) :: operands ) ->
      eval_line env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "line-points-dataset")) :: operands ) ->
      eval_line_points env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "num-by-num-plot")) :: operands ) ->
      eval_num_by_num env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "display")) :: operands ) ->
      eval_display env operands
  | Sexpr.Number (_, vl) -> Number vl
  | Sexpr.String (_, n) -> String n
  | Sexpr.Ident (l, n) ->
      begin
	try List.assoc n env.bindings
	with Not_found ->
	  failwith (sprintf
		      "line %d: Unknown identifier or malformed operation: %s"
		      l n)
      end
  | Sexpr.List (_, (Sexpr.Ident (l, i)) :: _) ->
      failwith (sprintf "line %d: Unknown or malformed operation: %s" l i)
  | Sexpr.List (l, _) ->
      failwith (sprintf "line %d: Unexpected list" l)


and eval_let env line = function
    (** [eval_let env line operands] evaluates the operands of a let
	expression. *)
  | Sexpr.List (_, bindings) :: (Sexpr.List (_, _) as expr) :: [] ->
      let binds =
	List.fold_right
	  (fun b l ->
	     match b with
	       | Sexpr.List (_, Sexpr.Ident (_, name) :: value :: []) ->
		   (name, eval env value) :: l
	       | e ->
		   failwith (sprintf "line %d: Malformed binding"
			       (Sexpr.line_number e)))
	  bindings []
      in
	eval {env with bindings = (binds @ env.bindings)} expr
  | e :: _ ->
      failwith (sprintf "line %d: Malformed let statement"
		  (Sexpr.line_number e))
  | [] ->
      failwith (sprintf "line %d: let expression: Unexpected end of file" line)


and eval_print env operands =
  (** [eval_print env operands] evaluates a print expression. *)
  List.fold_left
    (fun _ op ->
       let vl = eval env op in
	 begin match vl with
	   | Unit -> printf "()"
	   | Number vl -> printf "%f" vl
	   | String s -> printf "%s" s
	   | Scalars s ->
	       printf "[| "; Array.iter (printf "%f; ") s; printf " |]"
	   | Points s ->
	       printf "[| ";
	       Array.iter (fun p -> printf "(%f, %f); " p.x p.y) s;
	       printf "|]"
	   | Triples s ->
	       printf "[| ";
	       Array.iter (fun t -> printf "(%f, %f, %f); " t.i t.j t.k) s;
	       printf "|]"
	   | Num_by_num_dataset _ -> printf "<num-by-num-dataset>"
	   | Num_by_num_plot _ -> printf "<num-by-num-plot>"
	 end;
	 vl)
    Unit operands


and eval_points env operands =
  (** [eval_points env operands] evaluates a points list. *)
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
  in Points (Array.of_list lst)


and eval_triples env operands =
  (** [eval_triples env operands] evaluates a triples list. *)
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
  in Triples (Array.of_list lst)


and eval_color env line operands =
  (** [eval_color env line operands] evaluates a color. *)
  let r, g, b, a = match operands with
    | Sexpr.Number (_, r) :: Sexpr.Number (_, g)
      :: Sexpr.Number (_, b) :: [] ->
	r, g, b, 1.
    | Sexpr.Number (_, r) :: Sexpr.Number (_, g)
      :: Sexpr.Number (_, b) :: Sexpr.Number (_, a) :: [] ->
	r, g, b, a
    | _ -> failwith (sprintf "line %d: Malformed color" line)
  in Drawing.color ~r:r ~g:g ~b:b ~a:a


and eval_length env line = function
    (** [eval_length env operands] evaluates a length. *)
  | Sexpr.List (_, Sexpr.Ident (_, "in") :: Sexpr.Number (_, i) :: []) ->
      Length.In i
  | Sexpr.List (_, Sexpr.Ident (_, "cm") :: Sexpr.Number (_, c) :: []) ->
      Length.Cm c
  | Sexpr.List (_, Sexpr.Ident (_, "pt") :: Sexpr.Number (_, p) :: []) ->
      Length.Pt p
  | Sexpr.List (_, Sexpr.Ident (_, "px") :: Sexpr.Number (_, p) :: []) ->
      Length.Px (truncate p)
  | _ -> failwith (sprintf "line %d: Malformed length" line)


and eval_display env operands =
  List.fold_left
    (fun _ p -> match eval env p with
       | (Num_by_num_plot p) as vl -> p#display; vl
       | x ->
	   failwith (sprintf "line %d: Expected plot got %s"
		       (Sexpr.line_number p) (to_string x)))
    Unit operands;


(************************************************************)
(* Num-by-num                                               *)
(************************************************************)

and eval_num_by_num env operands =
  (** [eval_num_by_num env operands] evaluates a num_by_num plot. *)
  let module S = Sexpr in
  let title = ref None
  and xlabel = ref None
  and ylabel = ref None
  and x_min = ref None
  and x_max = ref None
  and y_min = ref None
  and y_max = ref None
  and datasets = ref []
  in
    List.iter
      (fun op -> match op with
	 | S.List (_, S.Ident (l, "title") :: S.String (_, t) :: []) ->
	     set_once title l "title" t
	 | S.List (_, S.Ident (l, "x-label") :: S.String (_, t) :: []) ->
	     set_once xlabel l "x-label" t
	 | S.List (_, S.Ident (l, "y-label") :: S.String (_, t) :: []) ->
	     set_once ylabel l "y-label" t
	 | S.List (_, S.Ident (l, "x-min") :: S.Number (_, t) :: []) ->
	     set_once x_min l "x-min" t
	 | S.List (_, S.Ident (l, "x-max") :: S.Number (_, t) :: []) ->
	     set_once x_max l "x-max" t
	 | S.List (_, S.Ident (l, "y-min") :: S.Number (_, t) :: []) ->
	     set_once y_min l "y-min" t
	 | S.List (_, S.Ident (l, "y-max") :: S.Number (_, t) :: []) ->
	     set_once y_max l "y-max" t
	 | S.List (l, dss) ->
	     datasets :=
	       !datasets @
		 (List.map
		    (fun e ->
		       match eval env e with
			 | Num_by_num_dataset ds -> ds
			 | x ->
			     failwith
			       (sprintf
				  "line %d: Expected num-by-num dataset got %s"
				  l (to_string x)))
		    dss);
	 | e ->
	     failwith (sprintf "line %d: Invalid option to a num-by-num plot"
			 (Sexpr.line_number e))
      ) operands;
    Num_by_num_plot
      (new Num_by_num.plot
	 ?title:!title
	 ?xlabel:!xlabel
	 ?ylabel:!ylabel
	 ?x_min:!x_min
	 ?x_max:!x_max
	 ?y_min:!y_min
	 ?y_max:!y_max
	 !datasets)


and eval_scatter env operands =
  (** [eval_scatter env operands] evaluates a scatter plot dataset. *)
  let module S = Sexpr in
  let glyph = ref None
  and color = ref None
  and radius = ref None
  and name = ref None
  and data = ref [| |]
  in
    List.iter
      (fun op -> match op with
	 | S.List (_, S.Ident (l, "name") :: S.String (_, t) :: []) ->
	     set_once name l "name" t
	 | S.List (_, S.Ident (l, "glyph") :: S.String (_, t) :: []) ->
	     set_once glyph l "glyph" (Drawing.glyph_of_string t)
	 | S.List (_, S.Ident (l, "color") :: operands) ->
	     set_once color l "color" (eval_color env l operands)
	 | S.List (_, S.Ident (l, "radius") :: len :: []) ->
	     set_once radius l "radius" (eval_length env l len)
	 | S.List (l, _) as points ->
	     begin match eval env points with
	       | Points pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected points got %s"
				  l (to_string x))
	     end
	 | e ->
	     failwith (sprintf "line %d: Invalid option to a scatter dataset"
			 (Sexpr.line_number e))
      ) operands;
    Num_by_num_dataset
      (new Num_by_num.scatter_dataset
	 (match !glyph with | Some g -> g | None -> env.next_glyph ())
	 ?color:!color
	 ?radius:!radius
	 ?name:!name
	 !data)


and eval_dashes env ds =
  let lst =
    List.map
      (function
	 | (Sexpr.List (l, _)) as len -> eval_length env l len
	 | x -> failwith (sprintf "line %d: Expected a length"
			    (Sexpr.line_number x)))
      ds
  in Array.of_list lst


and eval_line env operands =
  (** [eval_line env operands] evaluates a line dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and color = ref None
  and width = ref None
  and name = ref None
  and data = ref [| |]
  in
    List.iter
      (fun op -> match op with
	 | S.List (_, S.Ident (l, "name") :: S.String (_, t) :: []) ->
	     set_once name l "name" t
	 | S.List (_, S.Ident (l, "dashes") :: S.List (_, ds) :: []) ->
	       set_once dashes l "dashes" (eval_dashes env ds)
	 | S.List (_, S.Ident (l, "color") :: operands) ->
	     set_once color l "color" (eval_color env l operands)
	 | S.List (_, S.Ident (l, "width") :: len :: []) ->
	     set_once width l "width" (eval_length env l len)
	 | S.List (l, _) as points ->
	     begin match eval env points with
	       | Points pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected points got %s"
				  l (to_string x))
	     end
	 | e ->
	     failwith (sprintf "line %d: Invalid option to a scatter dataset"
			 (Sexpr.line_number e))
      ) operands;
    Num_by_num_dataset
      (new Num_by_num.line_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 ?color:!color
	 ?width:!width
	 ?name:!name
	 !data)


and eval_line_points env operands =
  (** [eval_line_points env operands] evaluates a line and points
      dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and glyph = ref None
  and radius = ref None
  and color = ref None
  and width = ref None
  and name = ref None
  and data = ref [| |]
  in
    List.iter
      (fun op -> match op with
	 | S.List (_, S.Ident (l, "name") :: S.String (_, t) :: []) ->
	     set_once name l "name" t
	 | S.List (_, S.Ident (l, "glyph") :: S.String (_, t) :: []) ->
	     set_once glyph l "glyph" (Drawing.glyph_of_string t)
	 | S.List (_, S.Ident (l, "dashes") :: S.List (_, ds) :: []) ->
	       set_once dashes l "dashes" (eval_dashes env ds)
	 | S.List (_, S.Ident (l, "color") :: operands) ->
	     set_once color l "color" (eval_color env l operands)
	 | S.List (_, S.Ident (l, "width") :: len :: []) ->
	     set_once width l "width" (eval_length env l len)
	 | S.List (_, S.Ident (l, "radius") :: len :: []) ->
	     set_once radius l "radius" (eval_length env l len)
	 | S.List (l, _) as points ->
	     begin match eval env points with
	       | Points pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected points got %s"
				  l (to_string x))
	     end
	 | e ->
	     failwith (sprintf "line %d: Invalid option to a scatter dataset"
			 (Sexpr.line_number e))
      ) operands;
    Num_by_num_dataset
      (Num_by_num.line_points_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 (match !glyph with | Some g -> g | None -> env.next_glyph ())
	 ?color:!color
	 ?width:!width
	 ?radius:!radius
	 ?name:!name
	 !data)
