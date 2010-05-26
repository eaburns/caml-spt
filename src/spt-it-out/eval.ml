(** Analyze s-experssions, conversion to plotting language and execution.

    @author eaburns
    @since 2010-05-14
*)

open Printf
open Geometry
open Verbosity

type t =
  | Unit
  | Number of float
  | String of string
  | Scalars of float array
  | Points of point array
  | Triples of triple array
  | Num_by_num_dataset of Num_by_num.dataset
  | Num_by_num_plot of Num_by_num.plot
  | Num_by_nom_dataset of Num_by_nom.dataset
  | Num_by_nom_plot of Num_by_nom.plot


type env = {
  bindings : (string * t) list;
  next_glyph : unit -> Drawing.glyph;
  next_dash : unit -> Length.t array;
  next_line_errbar : unit -> Line_errbar_dataset.style;
}

let init_env =
  let next_dash = Factories.default_dash_factory () in
    {
      bindings = [];
      next_glyph = Factories.default_glyph_factory ();
      next_dash = next_dash;
      next_line_errbar = Line_errbar_dataset.line_errbar_factory next_dash ();
    }


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
  | Num_by_nom_dataset _ -> "Num_by_nom_dataset"
  | Num_by_nom_plot _ -> "Num_by_nom_plot"


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
  | Sexpr.List (_, (Sexpr.Ident (_, "scalars")) :: operands ) ->
      eval_scalars env operands
  | Sexpr.List (_, (Sexpr.Ident (l, "scalars-file")) :: operands ) ->
      eval_scalars_file env l operands
  | Sexpr.List (_, (Sexpr.Ident (l, "scalars-cmd")) :: operands ) ->
      eval_scalars_cmd env l operands
  | Sexpr.List (_, (Sexpr.Ident (_, "points")) :: operands ) ->
      eval_points env operands
  | Sexpr.List (_, (Sexpr.Ident (l, "points-file")) :: operands ) ->
      eval_points_file env l operands
  | Sexpr.List (_, (Sexpr.Ident (l, "points-cmd")) :: operands ) ->
      eval_points_cmd env l operands
  | Sexpr.List (_, (Sexpr.Ident (_, "triples")) :: operands ) ->
      eval_triples env operands
  | Sexpr.List (_, (Sexpr.Ident (l, "triples-file")) :: operands ) ->
      eval_triples_file env l operands
  | Sexpr.List (_, (Sexpr.Ident (l, "triples-cmd")) :: operands ) ->
      eval_triples_cmd env l operands
  | Sexpr.List (_, (Sexpr.Ident (_, "scatter-dataset")) :: operands ) ->
      eval_scatter env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "bestfit-dataset")) :: operands ) ->
      eval_bestfit env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "bubble-dataset")) :: operands ) ->
      eval_bubble env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "line-dataset")) :: operands ) ->
      eval_line env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "line-points-dataset")) :: operands ) ->
      eval_line_points env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "line-errbar-dataset")) :: operands ) ->
      eval_line_errbar env operands
  | Sexpr.List (_, (Sexpr.Ident (l, "histogram-dataset")) :: operands ) ->
      eval_histogram env l operands
  | Sexpr.List (_, (Sexpr.Ident (l, "cdf-dataset")) :: operands ) ->
      eval_cdf env l operands
  | Sexpr.List (_, (Sexpr.Ident (_, "num-by-num-composite")) :: operands ) ->
      eval_num_by_num_composite env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "num-by-num-plot")) :: operands ) ->
      eval_num_by_num env operands
  | Sexpr.List (_, (Sexpr.Ident (line, "boxplot-dataset")) :: operands ) ->
      eval_boxplot env line operands
  | Sexpr.List (_, (Sexpr.Ident (_, "num-by-nom-plot")) :: operands ) ->
      eval_num_by_nom env operands
  | Sexpr.List (_, (Sexpr.Ident (_, "display")) :: operands ) ->
      eval_display env operands
  | Sexpr.List (_, (Sexpr.Ident (line, "output")) :: operands ) ->
      eval_output env line operands
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

(************************************************************)
(* Operations                                               *)
(************************************************************)


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


and eval_display env operands =
  List.fold_left
    (fun _ p -> match eval env p with
       | (Num_by_num_plot p) as vl -> p#display; vl
       | (Num_by_nom_plot p) as vl -> p#display; vl
       | x ->
	   failwith (sprintf "line %d: Expected plot got %s"
		       (Sexpr.line_number p) (to_string x)))
    Unit operands;


and eval_output env line = function
  | Sexpr.String(_, filename) :: plot_expr :: [] ->
      begin match eval env plot_expr with
	| (Num_by_num_plot p) as vl -> p#output filename; vl
	| (Num_by_nom_plot p) as vl -> p#output filename; vl
	| x ->
	    failwith (sprintf "line %d: Expected plot got %s"
			(Sexpr.line_number plot_expr) (to_string x))
      end
  | _ ->
      failwith (sprintf "line %d: Malformed output operation" line)


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
	   | Num_by_nom_dataset _ -> printf "<num-by-nom-dataset>"
	   | Num_by_nom_plot _ -> printf "<num-by-nom-plot>"
	 end;
	 vl)
    Unit operands

(************************************************************)
(* Types of data                                            *)
(************************************************************)

and eval_scalars env operands =
  (** [eval_scalars env operands] evaluates a scalar list. *)
  let lst =
    List.fold_right
      (fun p l -> match p with
	 | Sexpr.Number (_, x) ->  x :: l
	 | e ->
	     failwith (sprintf "line %d: Malformed scalar"
			 (Sexpr.line_number e)))
      operands []
  in Scalars (Array.of_list lst)


and eval_scalars_file env line = function
    (** [eval_scalars_file env operands] evaluates a scalar file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let floats = read_floats inch in
	close_in inch;
	floats
  | _ -> failwith (sprintf "line %d: Malformed scalar-file expression"
		     line)

and eval_scalars_cmd env line = function
    (** [eval_scalars_cmd env operands] evaluates a scalar command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let floats = read_floats inch in
	ignore (Unix.close_process_in inch);
	floats
  | _ -> failwith (sprintf "line %d: Malformed scalar-cmd expression"
		     line)


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


and eval_points_file env line = function
    (** [eval_points_file env operands] evaluates a points file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let points = read_points inch in
	close_in inch;
	points
  | _ -> failwith (sprintf "line %d: Malformed points-file expression"
		     line)

and eval_points_cmd env line = function
    (** [eval_points_cmd env operands] evaluates a points command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let points = read_points inch in
	ignore (Unix.close_process_in inch);
	points
  | _ -> failwith (sprintf "line %d: Malformed points-cmd expression"
		     line)


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


and eval_triples_file env line = function
    (** [eval_triples_file env operands] evaluates a triples file. *)
  | Sexpr.String (l, file) :: [] ->
      let inch = open_in file in
      let points = read_triples inch in
	close_in inch;
	points
  | _ -> failwith (sprintf "line %d: Malformed triples-file expression"
		     line)

and eval_triples_cmd env line = function
    (** [eval_triples_cmd env operands] evaluates a triples
	command. *)
  | Sexpr.String (l, cmd) :: [] ->
      let inch = Unix.open_process_in cmd in
      let points = read_triples inch in
	ignore (Unix.close_process_in inch);
	points
  | _ -> failwith (sprintf "line %d: Malformed triples-cmd expression"
		     line)

(************************************************************)
(* Plot look-and-feel items.                                *)
(************************************************************)

and eval_dashes env ds =
  let lst =
    List.map
      (function
	 | (Sexpr.List (l, _)) as len -> eval_length env len
	 | x -> failwith (sprintf "line %d: Expected a length"
			    (Sexpr.line_number x)))
      ds
  in Array.of_list lst


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


and eval_length env = function
    (** [eval_length env operands] evaluates a length. *)
  | Sexpr.List (_, Sexpr.Ident (_, "in") :: Sexpr.Number (_, i) :: []) ->
      Length.In i
  | Sexpr.List (_, Sexpr.Ident (_, "cm") :: Sexpr.Number (_, c) :: []) ->
      Length.Cm c
  | Sexpr.List (_, Sexpr.Ident (_, "pt") :: Sexpr.Number (_, p) :: []) ->
      Length.Pt p
  | Sexpr.List (_, Sexpr.Ident (_, "px") :: Sexpr.Number (_, p) :: []) ->
      Length.Px (truncate p)
  | x -> failwith (sprintf "line %d: Malformed length" (Sexpr.line_number x))


and eval_legend_loc env = function
  | Sexpr.Ident (l, "upper-right") -> Legend.Upper_right
  | Sexpr.Ident (l, "upper-left") -> Legend.Upper_left
  | Sexpr.Ident (l, "lower-right") -> Legend.Lower_right
  | Sexpr.Ident (l, "lower-left") -> Legend.Lower_left
  | Sexpr.List (_, Sexpr.Ident(_, "at") :: Sexpr.Ident(l, txt_loc)
		  :: Sexpr.Number(_, x) :: Sexpr.Number(_, y) :: []) ->
      let txt_loc = match String.lowercase txt_loc with
	| "text-before" -> Legend.Text_before
	| "text-after" -> Legend.Text_after
	| _ -> failwith (sprintf
			   "line %d: Malformed text location %s"
			   l "try one of 'text-before' or 'text-after'")
      in Legend.At (txt_loc, x, y)
  | x -> failwith (sprintf "line %d: Malformed legend location"
		     (Sexpr.line_number x))

(************************************************************)
(* Num-by-num                                               *)
(************************************************************)

and eval_num_by_num env operands =
  (** [eval_num_by_num env operands] evaluates a num_by_num plot. *)
  let module S = Sexpr in
  let title = ref None
  and legend_loc = ref None
  and xlabel = ref None
  and ylabel = ref None
  and x_min = ref None
  and x_max = ref None
  and y_min = ref None
  and y_max = ref None
  and width = ref None
  and height = ref None
  and datasets = ref []
  in
    List.iter
      (fun op -> match op with
	 | S.List (_, S.Ident (l, "legend-location") :: loc :: []) ->
	     set_once legend_loc l "legend-location" (eval_legend_loc env loc)
	 | S.List (_, S.Ident (l, "title") :: S.String (_, t) :: []) ->
	     set_once title l "title" t
	 | S.List (_, S.Ident (l, "x-label") :: S.String (_, t) :: []) ->
	     set_once xlabel l "x-label" t
	 | S.List (_, S.Ident (l, "y-label") :: S.String (_, t) :: []) ->
	     set_once ylabel l "y-label" t
	 | S.List (_, S.Ident (l, "width") :: len :: []) ->
	     set_once width l "width" (eval_length env len)
	 | S.List (_, S.Ident (l, "height") :: len :: []) ->
	     set_once height l "height" (eval_length env len)
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
    let plot = (new Num_by_num.plot ?title:!title ?xlabel:!xlabel
		  ?legend_loc:!legend_loc
		  ?ylabel:!ylabel ?x_min:!x_min ?x_max:!x_max
		  ?y_min:!y_min ?y_max:!y_max !datasets)
    in
    let width = match !width with None -> plot#width | Some w -> w in
    let height = match !height with None -> plot#height | Some h -> h in
      plot#set_size ~w:width ~h:height;
      Num_by_num_plot plot


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
	     set_once radius l "radius" (eval_length env len)
	 | points ->
	     begin match eval env points with
	       | Points pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected points got %s"
				  (S.line_number points) (to_string x))
	     end
      ) operands;
    Num_by_num_dataset
      (new Num_by_num.scatter_dataset
	 (match !glyph with | Some g -> g | None -> env.next_glyph ())
	 ?color:!color
	 ?radius:!radius
	 ?name:!name
	 !data)


and eval_bestfit env operands =
  (** [eval_bestfit env operands] evaluates a line-of-bestfit
      dataset. *)
  let module S = Sexpr in
  let glyph = ref None
  and dashes = ref None
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
	 | S.List (_, S.Ident (l, "dashes") :: S.List (_, ds) :: []) ->
	     set_once dashes l "dashes" (eval_dashes env ds)
	 | S.List (_, S.Ident (l, "color") :: operands) ->
	     set_once color l "color" (eval_color env l operands)
	 | S.List (_, S.Ident (l, "radius") :: len :: []) ->
	     set_once radius l "radius" (eval_length env len)
	 | points ->
	     begin match eval env points with
	       | Points pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected points got %s"
				  (S.line_number points) (to_string x))
	     end
      ) operands;
    Num_by_num_dataset
      (Num_by_num.bestfit_dataset
	 (match !glyph with | Some g -> g | None -> env.next_glyph ())
	 (match !dashes with | Some d -> d | None -> env.next_dash ())
	 ?color:!color
	 ?radius:!radius
	 ?name:!name
	 !data)


and eval_bubble env operands =
  (** [eval_bubble env operands] evaluates a bubble plot dataset. *)
  let module S = Sexpr in
  let glyph = ref None
  and color = ref None
  and min_radius = ref None
  and max_radius = ref None
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
	 | S.List (_, S.Ident (l, "min-radius") :: len :: []) ->
	     set_once min_radius l "min-radius" (eval_length env len)
	 | S.List (_, S.Ident (l, "max-radius") :: len :: []) ->
	     set_once max_radius l "max-radius" (eval_length env len)
	 | triples ->
	     begin match eval env triples with
	       | Triples pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected triples got %s"
				  (S.line_number triples) (to_string x))
	     end
      ) operands;
    Num_by_num_dataset
      (new Num_by_num.bubble_dataset
	 ?glyph:!glyph
	 ?color:!color
	 ?min_radius:!min_radius
	 ?max_radius:!max_radius
	 ?name:!name
	 !data)


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
	     set_once width l "width" (eval_length env len)
	 | points ->
	     begin match eval env points with
	       | Points pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected points got %s"
				  (S.line_number points) (to_string x))
	     end
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
	     set_once width l "width" (eval_length env len)
	 | S.List (_, S.Ident (l, "radius") :: len :: []) ->
	     set_once radius l "radius" (eval_length env len)
	 | points ->
	     begin match eval env points with
	       | Points pts ->
		   data := Array.append !data pts
	       | x -> failwith (sprintf "line %d: Expected points got %s"
				  (S.line_number points) (to_string x))
	     end
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


and eval_line_errbar env operands =
  (** [eval_line_errbar env operands] evaluates a line and error bar
      dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and color = ref None
  and width = ref None
  and name = ref None
  and data = ref [ ]
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
	     set_once width l "width" (eval_length env len)
	 | S.List (l, point_sets) ->
	     List.iter
	       (fun points -> match eval env points with
		  | Points pts ->
		      data := pts :: !data
		  | x -> failwith (sprintf "line %d: Expected points got %s"
				     (Sexpr.line_number points) (to_string x)))
	       point_sets
	 | e ->
	     failwith (sprintf
			 "line %d: Invalid option to a line-errbar dataset"
			 (Sexpr.line_number e))
      ) operands;
    let style = match !dashes with
      | Some d -> { (env.next_line_errbar ())
		    with Line_errbar_dataset.dashes = d }
      | None -> env.next_line_errbar ()
    in
      Num_by_num_dataset
	(new Num_by_num.line_errbar_dataset
	   style
	   ?color:!color
	   ?width:!width
	   ?name:!name
	   (Array.of_list !data))


and eval_histogram env line operands =
  (** [eval_histogram env line operands] evaluates a histogram
      dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and color = ref None
  and width = ref None
  and bin_width = ref None
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
	     set_once width l "width" (eval_length env len)
	 | S.List (_, S.Ident (l, "bin-width") :: S.Number(_, w) :: []) ->
	     set_once bin_width l "bin-width" w
	 | floats ->
	     begin match eval env floats with
	       | Scalars vls ->
		   data := Array.append !data vls
	       | x -> failwith (sprintf "line %d: Expected scalars got %s"
				  (S.line_number floats) (to_string x))
	     end
      ) operands;
    Num_by_num_dataset
      (new Num_by_num.histogram_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 ?width:!width ?color:!color ?bin_width:!bin_width ?name:!name
	 !data)


and eval_cdf env line operands =
  (** [eval_cdf env line operands] evaluates a cumulative density
      dataset. *)
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
	     set_once width l "width" (eval_length env len)
	 | floats ->
	     begin match eval env floats with
	       | Scalars vls ->
		   data := Array.append !data vls
	       | x -> failwith (sprintf "line %d: Expected scalars got %s"
				  (S.line_number floats) (to_string x))
	     end
      ) operands;
    Num_by_num_dataset
      (new Num_by_num.cdf_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 ?width:!width ?color:!color ?name:!name !data)


and eval_num_by_num_composite env operands =
  (** [eval_num_by_num_composite env operands] evaluates a composite
      num-by-num dataset. *)
  let module S = Sexpr in
  let name = ref None
  and dss = ref [] in
    List.iter
      (function
	 | S.List (_, S.Ident (l, "name") :: S.String (_, n) :: []) ->
	     set_once name l "name" n
	 | S.List (_, datasets) ->
	     dss := !dss @
	       (List.map
		  (fun expr -> match eval env expr with
		     | Num_by_num_dataset ds -> ds
		     | e ->
			 failwith
			   (sprintf
			      "line %d: Expected a num-by-num dataset got %s"
			      (Sexpr.line_number expr) (to_string e))
		  )
		  datasets)
	 | e ->
	     failwith
	       (sprintf
		  "line %d: Invalid option to a num-by-num-composite dataset"
		  (Sexpr.line_number e)))
      operands;
    Num_by_num_dataset
      (new Num_by_num.composite_dataset ?name:!name (List.rev !dss))

(************************************************************)
(* Num-by-nom                                               *)
(************************************************************)

and eval_num_by_nom env operands =
  (** [eval_num_by_nom env operands] evaluates a num_by_nom plot. *)
  let module S = Sexpr in
  let title = ref None
  and ylabel = ref None
  and y_min = ref None
  and y_max = ref None
  and width = ref None
  and height = ref None
  and datasets = ref []
  in
    List.iter
      (fun op -> match op with
	 | S.List (_, S.Ident (l, "title") :: S.String (_, t) :: []) ->
	     set_once title l "title" t
	 | S.List (_, S.Ident (l, "y-label") :: S.String (_, t) :: []) ->
	     set_once ylabel l "y-label" t
	 | S.List (_, S.Ident (l, "y-min") :: S.Number (_, t) :: []) ->
	     set_once y_min l "y-min" t
	 | S.List (_, S.Ident (l, "y-max") :: S.Number (_, t) :: []) ->
	     set_once y_max l "y-max" t
	 | S.List (_, S.Ident (l, "width") :: len :: []) ->
	     set_once width l "width" (eval_length env len)
	 | S.List (_, S.Ident (l, "height") :: len :: []) ->
	     set_once height l "height" (eval_length env len)
	 | S.List (l, dss) ->
	     datasets :=
	       !datasets @
		 (List.map
		    (fun e ->
		       match eval env e with
			 | Num_by_nom_dataset ds -> ds
			 | x ->
			     failwith
			       (sprintf
				  "line %d: Expected num-by-nom dataset got %s"
				  l (to_string x)))
		    dss);
	 | e ->
	     failwith (sprintf "line %d: Invalid option to a num-by-nom plot"
			 (Sexpr.line_number e))
      ) operands;
    let plot = (new Num_by_nom.plot ?title:!title ?ylabel:!ylabel
		  ?y_min:!y_min ?y_max:!y_max !datasets)
    in
    let width = match !width with None -> plot#width | Some w -> w in
    let height = match !height with None -> plot#height | Some h -> h in
      plot#set_size ~w:width ~h:height;
      Num_by_nom_plot plot


and eval_boxplot env line operands =
  (** [eval_boxplot env line operands] evaluates a boxplot dataset. *)
  let module S = Sexpr in
  let radius = ref None
  and name = ref None
  and data = ref [| |]
  in
    List.iter
      (fun op -> match op with
	 | S.List (_, S.Ident (l, "name") :: S.String (_, t) :: []) ->
	     set_once name l "name" t
	 | S.List (_, S.Ident (l, "radius") :: len :: []) ->
	     set_once radius l "radius" (eval_length env len)
	 | floats ->
	     begin match eval env floats with
	       | Scalars vls ->
		   data := Array.append !data vls
	       | x -> failwith (sprintf "line %d: Expected scalars got %s"
				  (S.line_number floats) (to_string x))
	     end
      ) operands;
    match !name with
      | None ->
	  failwith
	    (sprintf "line %d: Invalid boxplot dataset, no name given" line)
      | Some n ->
	  Num_by_nom_dataset
	    (new Num_by_nom.boxplot_dataset ?radius:!radius n !data)
