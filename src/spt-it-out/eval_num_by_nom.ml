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
	  printf "line %d: Invalid boxplot dataset, no name given\n" line;
	  raise (Evaluate.Invalid_argument line)
      | Some n ->
	  Num_by_nom_dataset
	    (Num_by_nom.boxplot_dataset ?point_radius:!radius n !data)

let help_str_boxplot =
  "(boxplot-dataset :name <string> [:point-radius <length>])\n\
 :values <scalars>)\n\
Creates a new boxplot dataset with the given name over the given\n\
values.  The :point-radius option specifies the size of the outlier\n\
points."


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
	  printf "line %d: Invalid barchart, no name given\n" line;
	  raise (Evaluate.Invalid_argument line)
      | Some n -> n
    and data = match !data with
      | None ->
	  printf "line %d: Invalid barchart, no value given\n" line;
	  raise (Evaluate.Invalid_argument line)
      | Some n -> n
    in
      Num_by_nom_dataset
	(Num_by_nom.barchart_dataset (env.next_fill()) name data)

let help_str_barchart =
  "(barchart-dataset :name <string> :value <number>)\n\
Creates a new barchart dataset.  This will create a single bar with\n\
the given name and value."

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
	  printf "line %d: Invalid barchart with errorbars dataset, %s\n"
	    line "no name given";
	  raise (Evaluate.Invalid_argument line)
      | Some n ->
	  Num_by_nom_dataset
	    (Num_by_nom.barchart_errbar_dataset (env.next_fill()) n !data)


let help_str_barchart_errbar =
  "(barchart-errbar-dataset :name <string> :values <scalars>)\n\
Creates a new barchart dataset with error bars.  The height\n\
of the bar is the mean value of :values and the error bars\n\
show the 95% confidence intervals."


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
	 | Num_by_nom_dataset ds -> datasets := ds :: !datasets
	 | x ->
	     printf "line %d: Expected num-by-nom dataset got %s\n"
	       l (value_to_string x);
	     raise (Evaluate.Invalid_argument l))
  ]
  in
    Options.handle opts operands;
    let plot = (Num_by_nom.plot ?title:!title ?ylabel:!ylabel
		  ?y_min:!y_min ?y_max:!y_max !datasets)
    in
    let width = match !width with None -> plot#width | Some w -> w in
    let height = match !height with None -> plot#height | Some h -> h in
      plot#set_size ~w:width ~h:height;
      Num_by_nom_plot plot


let help_str_num_by_nom_plot =
  "(num-by-nom-plot [:title <string>] [:y-label <string>]\n\
 [:width <length>] [:height <length>] [:y-max <number>]\n\
 [:y-min <number>] [:dataset <dataset>]+)\n\
Creates a new plot with a numeric y-axis and a nominal x-axis."


let functions = [
  "boxplot-dataset", eval_boxplot, help_str_boxplot;
  "barchart-dataset", eval_barchart, help_str_barchart;
  "barchart-errbar-dataset", eval_barchart_errbar, help_str_barchart_errbar;
  "num-by-nom-plot", eval_num_by_nom_plot, help_str_num_by_nom_plot;
]
