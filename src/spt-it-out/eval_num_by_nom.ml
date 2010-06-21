(** Expression evaluation for numeric by nominal plots.

    @author eaburns
    @since 2010-06-21
*)

open Printf
open Evaluate


let eval_boxplot eval_rec env line operands =
  (** [eval_boxplot eval_rec env line operands] evaluates a boxplot
      dataset. *)
  let module S = Sexpr in
  let radius = ref None and name = ref None and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.length_option_ref ":point-radius" radius;
    ":values", Options.Expr (fun l e ->
			       let s = Eval_data.scalars eval_rec env e in
				 data := Array.append !data s)
  ]
  in
    Options.handle opts operands;
    match !name with
      | None ->
	  failwith
	    (sprintf "line %d: Invalid boxplot dataset, no name given" line)
      | Some n ->
	  `Num_by_nom_dataset
	    (Num_by_nom.boxplot_dataset ?point_radius:!radius n !data)


let eval_barchart eval_rec env line operands =
  (** [eval_barchart eval_rec env line operands] evaluates a barchart
      dataset. *)
  let module S = Sexpr in
  let name = ref None and data = ref None in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.number_option_ref ":value" data;
  ] in
    Options.handle opts operands;
    let name = match !name with
      | None ->
	  failwith (sprintf "line %d: Invalid barchart, no name given" line)
      | Some n -> n
    and data = match !data with
      | None ->
	  failwith (sprintf "line %d: Invalid barchart, no value given" line)
      | Some n -> n
    in
      `Num_by_nom_dataset
	(Num_by_nom.barchart_dataset (env.next_fill()) name data)


let eval_barchart_errbar eval_rec env line operands =
  (** [eval_barchart_errbar eval_rec env line operands] evaluates a
      barchart with error bars dataset. *)
  let module S = Sexpr in
  let name = ref None and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    ":values", Options.Expr (fun l e ->
			       let s = Eval_data.scalars eval_rec env e in
				 data := Array.append !data s)
  ]
  in
    Options.handle opts operands;
    match !name with
      | None ->
	  failwith
	    (sprintf "line %d: Invalid barchart with errorbars dataset, %s"
	       line "no name given")
      | Some n ->
	  `Num_by_nom_dataset
	    (Num_by_nom.barchart_errbar_dataset (env.next_fill()) n !data)


let eval_num_by_nom_plot eval_rec env line operands =
  (** [eval_num_by_nom_plot eval_rec env line operands] evaluates a
      num_by_nom plot. *)
  let module S = Sexpr in
  let title = ref None
  and ylabel = ref None
  and y_min = ref None
  and y_max = ref None
  and width = ref None
  and height = ref None
  and datasets = ref [] in
  let opts = [
    Options.string_option_ref ":title" title;
    Options.string_option_ref ":y-label" ylabel;
    Options.length_option_ref ":width" width;
    Options.length_option_ref ":height" height;
    Options.number_option_ref ":y-min" y_min;
    Options.number_option_ref ":y-max" y_max;
    ":dataset", Options.Expr
      (fun l e -> match eval_rec env e with
	 | `Num_by_nom_dataset ds -> datasets := ds :: !datasets
	 | x ->
	     failwith (sprintf "line %d: Expected num-by-nom dataset got %s"
			 l (value_to_string x)))
  ]
  in
    Options.handle opts operands;
    let plot = (Num_by_nom.plot ?title:!title ?ylabel:!ylabel
		  ?y_min:!y_min ?y_max:!y_max !datasets)
    in
    let width = match !width with None -> plot#width | Some w -> w in
    let height = match !height with None -> plot#height | Some h -> h in
      plot#set_size ~w:width ~h:height;
      `Num_by_nom_plot plot



let functions = [
  "boxplot-dataset", eval_boxplot;
  "barchart-dataset", eval_barchart;
  "barchart-errbar-dataset", eval_barchart_errbar;
  "num-by-nom-plot", eval_num_by_nom_plot;
]
