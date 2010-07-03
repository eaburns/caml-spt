(**

   @author eaburns
   @since 2010-06-19
*)

open Printf
open Evaluate

let eval_legend_location eval_rec env line = function
    (** [eval_legend_location eval_rec env line] evaluates a legend
    location. *)
  | Sexpr.Ident (l, ":upper-right") :: [] -> Legend_loc Legend.Upper_right
  | Sexpr.Ident (l, ":upper-left") :: [] -> Legend_loc Legend.Upper_left
  | Sexpr.Ident (l, ":lower-right") :: [] -> Legend_loc Legend.Lower_right
  | Sexpr.Ident (l, ":lower-left") :: [] -> Legend_loc Legend.Lower_left
  | Sexpr.List (_, Sexpr.Ident(_, "legend-at") :: Sexpr.Ident(l, txt_loc)
		  :: Sexpr.Number(_, x) :: Sexpr.Number(_, y) :: []) :: [] ->
      let txt_loc = match String.lowercase txt_loc with
	| ":text-before" -> Legend.Text_before
	| ":text-after" -> Legend.Text_after
	| _ ->
	    printf "line %d: Malformed text location %s\n"
	      l "try one of ':text-before' or ':text-after'";
	    raise (Invalid_argument l)
      in Legend_loc (Legend.At (txt_loc, x, y))
  | x :: _ ->
      printf "line %d: Malformed legend location\n" (Sexpr.line_number x);
      raise (Invalid_argument (Sexpr.line_number x))
  | [] ->
      printf "line %d: Malformed legend location\n" line;
      raise (Invalid_argument line)


let help_str_legend_location =
  "(legend-location <legend-loc>)\n\
Creates a legend location, one of:\n\
\t:upper-right\n\
\t:upper-left\n\
\t:lower-right\n\
\t:lower-left\n\
\t(legend-at [:text-before|:text-after] <number> <number>)"


let eval_scatter eval_rec env line operands =
  (** [eval_scatter eval_rec env line operands] evaluates a scatter
      plot dataset. *)
  let module S = Sexpr in
  let glyph = ref None and color = ref None and radius = ref None
					    and name = ref None and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.glyph glyph;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":point-radius" radius;
    ":points", Options.List (fun l e ->
			       let p = Eval_data.points eval_rec env l e in
				 data := Array.append !data p);
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.scatter_dataset
	 (match !glyph with | Some g -> g | None -> env.next_glyph ())
	 ?color:!color
	 ?point_radius:!radius
	 ?name:!name
	 !data)


let help_str_scatter =
  "(scatter-dataset [:name <string>] [:glyph <string>] [:color <color>]\n\
 [:point-radius <length>] [:points <points>]+)\n\
Creates a scatter plot datatset of the sets of points.  All points\n\
in the dataset use the same glyph.  If specified :point-radius\n\
selects the size of the glyphs."


let eval_bestfit eval_rec env line operands =
  (** [eval_bestfit eval_rec env line operands] evaluates a
      line-of-bestfit dataset. *)
  let module S = Sexpr in
  let glyph = ref None
  and dashes = ref None
  and color = ref None
  and radius = ref None
  and name = ref None
  and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.glyph glyph;
    Options.dashes eval_rec env dashes;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":point-radius" radius;
    ":points", Options.List (fun l e ->
			       let p = Eval_data.points eval_rec env l e in
				 data := Array.append !data p);
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.bestfit_dataset
	 ~glyph:(match !glyph with | Some g -> g | None -> env.next_glyph ())
	 ~dashes:(match !dashes with | Some d -> d | None -> env.next_dash ())
	 ?color:!color
	 ?point_radius:!radius
	 ?name:!name
	 !data)


let help_str_bestfit =
  "(bestfit-dataset [:name <string>] [:glyph <string>] [:dashes <dashes>]
 [:color <color>] [:point-radius <length>] [:points <points>]+)\n\
Creates a scatter plot datatset with a bestfit line.  All points\n\
in the dataset use the same glyph.  The :dashes option sets the dash\n\
pattern for the bestfit line.  If specified :point-radius selects\n\
the size of the glyphs."


let eval_bubble eval_rec env line operands =
  (** [eval_bubble eval_rec env line operands] evaluates a bubble plot
      dataset. *)
  let module S = Sexpr in
  let glyph = ref None
  and color = ref None
  and min_radius = ref None
  and max_radius = ref None
  and name = ref None
  and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.glyph glyph;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":min-radius" min_radius;
    Options.length_option_ref eval_rec env ":max-radius" max_radius;
    ":triples", Options.List (fun l e ->
				let t = Eval_data.triples eval_rec env l e in
				  data := Array.append !data t);
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.bubble_dataset
	 ?glyph:!glyph
	 ?color:!color
	 ?min_radius:!min_radius
	 ?max_radius:!max_radius
	 ?name:!name
	 !data)

let help_str_bubble =
  "(bubble-dataset [:name <string>] [:glyph <string>] [:color <color>]\n\
 [:min-radius <length>] [:max-radius <length>] [:triples <triples>]+)\n\
Creates a bubble plot datatset of the sets of points.  A bubble\n\
plot is like a scatter plot except that each point is given a size\n\
between :min-radius and :max-radius based on the third value in\n\
each triple."


let eval_line eval_rec env line operands =
  (** [eval_line eval_rec env line operands] evaluates a line
      dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and color = ref None
  and width = ref None
  and name = ref None
  and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.dashes eval_rec env dashes;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":line-width" width;
    ":points", Options.List (fun l e ->
			       let p = Eval_data.points eval_rec env l e in
			       data := Array.append !data p);
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.line_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 ?color:!color
	 ?line_width:!width
	 ?name:!name
	 !data)

let help_str_line =
  "(line-dataset [:name <string>] [:dashes <dashes>] [:color <color>]\n\
 [:line-width <length>] [:points <points>]+)\n\
Creates a line datatset. The :dashes option sets the dash pattern\n\
for the line.  If specified :line-width selects the width of the line."


let eval_line_points eval_rec env line operands =
  (** [eval_line_points eval_rec env line operands] evaluates a line and points
      dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and glyph = ref None
  and radius = ref None
  and color = ref None
  and width = ref None
  and name = ref None
  and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.dashes eval_rec env dashes;
    Options.glyph glyph;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":line-width" width;
    Options.length_option_ref eval_rec env ":point-radius" radius;
    ":points", Options.List (fun l e ->
			       let p = Eval_data.points eval_rec env l e in
				 data := Array.append !data p);
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.line_points_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 (match !glyph with | Some g -> g | None -> env.next_glyph ())
	 ?color:!color
	 ?line_width:!width
	 ?point_radius:!radius
	 ?name:!name
	 !data)


let help_str_line_points =
  "(line-points-dataset [:name <string>] [:dashes <dashes>]\n\
 [:color <color>] [:point-radius <length>] [:line-width <length>]\n\
 [:points <points>]+)\n\
Creates a line datatset with a glyph at each specified point. The\n\
:dashes option sets the dash pattern for the line."


let eval_line_errbar eval_rec env line operands =
  (** [eval_line_errbar eval_rec env line operands] evaluates a line
      and error bar dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and color = ref None
  and width = ref None
  and name = ref None
  and data = ref [ ] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.dashes eval_rec env dashes;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":line-width" width;
    ":lines",
    Options.List
      (fun l lines ->
	 Array.iter (function
		       | List lst ->
			   let p = Eval_data.points eval_rec env l lst in
			     data := p :: !data
		       | x ->
			   printf "line %d: Expected points, got %s\n"
			     l (value_name x);
			   raise (Invalid_argument l);)
	   lines);
  ] in
    Options.handle eval_rec env opts operands;
    let style = match !dashes with
      | Some d -> { (env.next_line_errbar ()) with Num_by_num.dashes = d }
      | None -> env.next_line_errbar ()
    in
      Num_by_num_dataset
	(Num_by_num.line_errbar_dataset
	   style
	   ?color:!color
	   ?line_width:!width
	   ?name:!name
	   (Array.of_list !data))


let help_str_line_errbar =
  "(line-errbar-dataset [:name <string>] [:dashes <dashes>]\n\
 [:color <color>] [:line-width <length>] [:lines (<points>+)]+)\n\
Creates a line datatset error bars.  Each set of points given by\n\
the :lines option is taken to be an individual line.  The line\n\
drawn by this dataset is the interpolated mean line for the given\n\
lines.  Error bars are drawn to show the 95% confidence intervals."



let eval_histogram eval_rec env line operands =
  (** [eval_histogram eval_rec env line operands] evaluates a histogram
      dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and color = ref None
  and width = ref None
  and bin_width = ref None
  and name = ref None
  and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.dashes eval_rec env dashes;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":line-width" width;
    Options.number_option_ref ":bin-width" bin_width;
    ":values", Options.List (fun l e ->
			       let s = Eval_data.scalars eval_rec env l e in
				 data := Array.append !data s)
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.histogram_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 ?line_width:!width ?bg_color:!color ?bin_width:!bin_width ?name:!name
	 !data)


let help_str_histogram =
  "(histogram-dataset [:name <string>] [:line-width <length>]\n\
 [:bin-width <number>] [:values <scalars>]+)\n\
Creates a histogram of the given values."


let eval_cdf eval_rec env line operands =
  (** [eval_cdf eval_rec env line operands] evaluates a cumulative
      density dataset. *)
  let module S = Sexpr in
  let dashes = ref None
  and color = ref None
  and width = ref None
  and name = ref None
  and data = ref [| |] in
  let opts = [
    Options.string_option_ref ":name" name;
    Options.dashes eval_rec env dashes;
    Options.color eval_rec env color;
    Options.length_option_ref eval_rec env ":line-width" width;
    ":values", Options.List (fun l e ->
			       let s = Eval_data.scalars eval_rec env l e in
				 data := Array.append !data s)
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.cdf_dataset
	 (match !dashes with | Some g -> g | None -> env.next_dash ())
	 ?line_width:!width ?color:!color ?name:!name !data)


let help_str_cdf =
  "(cdf-dataset [:name <string>] [:line-width <length>]\n\
 [:values <scalars>]+)\n\
Creates a cumulative density plot of the given values."



let eval_num_by_num_composite eval_rec env line operands =
  (** [eval_num_by_num_composite eval_rec env line operands] evaluates
      a composite num-by-num dataset. *)
  let module S = Sexpr in
  let name = ref None and dss = ref [] in
  let opts = [
    Options.string_option_ref ":name" name;
    ":dataset",
    Options.Expr (fun l e -> match eval_rec env e with
		    | Num_by_num_dataset ds -> dss := ds :: !dss
		    | x ->
			printf "line %d: Expected num-by-num dataset got %s\n"
			  l (value_name x);
			raise (Evaluate.Invalid_argument l));
  ] in
    Options.handle eval_rec env opts operands;
    Num_by_num_dataset
      (Num_by_num.composite_dataset ?name:!name (List.rev !dss))


let help_str_num_by_num_composite =
  "(num-by-num-composite [:name <string>] [:dataset <num-by-num-dataset]+)\n\
Creates a single numeric by numbeic dataset that is the composite of\n\
the given datasets."


let eval_num_by_num_plot eval_rec env line operands =
  (** [eval_num_by_num_plot eval_rec env line operands] evaluates a
      num_by_num plot. *)
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
  and sort_legend = ref true
  and datasets = ref [] in
  let opts = [
    Options.legend eval_rec env legend_loc;
    Options.string_option_ref ":title" title;
    Options.string_option_ref ":x-label" xlabel;
    Options.string_option_ref ":y-label" ylabel;
    Options.length_option_ref eval_rec env ":width" width;
    Options.length_option_ref eval_rec env ":height" height;
    Options.number_option_ref ":x-min" x_min;
    Options.number_option_ref ":x-max" x_max;
    Options.number_option_ref ":y-min" y_min;
    Options.number_option_ref ":y-max" y_max;
    Options.bool_ref ":sort-legend" sort_legend;
    ":dataset", Options.Expr
      (fun l e -> match eval_rec env e with
	 | Num_by_num_dataset ds -> datasets := ds :: !datasets
	 | x ->
	     printf "line %d: Expected num-by-num dataset got %s\n"
	       l (value_name x);
	     raise (Evaluate.Invalid_argument l))
  ]
  in
    Options.handle eval_rec env opts operands;
    let plot = (Num_by_num.plot ?title:!title ?xlabel:!xlabel
		  ~sort_legend:!sort_legend
		  ?legend_loc:!legend_loc
		  ?ylabel:!ylabel ?x_min:!x_min ?x_max:!x_max
		  ?y_min:!y_min ?y_max:!y_max (List.rev !datasets))
    in
    let width = match !width with None -> plot#width | Some w -> w in
    let height = match !height with None -> plot#height | Some h -> h in
      plot#set_size ~w:width ~h:height;
      Num_by_num_plot plot


let help_str_num_by_num_plot =
  "(num-by-num-plot [:title <string>] [:x-label <string>]\n\
 [:y-label <string>] [:width <length>] [:height <length>]\n\
 [:x-min <number>] [:x-max <number>] [:y-min <number>] [:y-max <number>]\n\
 [:sort-legend <bool>] [:legend <legend location>]\n\
 [:dataset <num-by-num-dataset>]+)\n\
Creates a plot with numeric x and y axes."


let functions = [
  "legend-location", eval_legend_location, help_str_legend_location;
  "scatter-dataset", eval_scatter, help_str_scatter;
  "bestfit-dataset", eval_bestfit, help_str_bestfit;
  "bubble-dataset", eval_bubble, help_str_bubble;
  "line-dataset", eval_line, help_str_line;
  "line-points-dataset", eval_line_points, help_str_line_points;
  "line-errbar-dataset", eval_line_errbar, help_str_line_errbar;
  "histogram-dataset", eval_histogram, help_str_histogram;
  "cdf-dataset", eval_cdf, help_str_cdf;

  "num-by-num-composite",
  eval_num_by_num_composite,
  help_str_num_by_num_composite;

  "num-by-num-plot", eval_num_by_num_plot, help_str_num_by_num_plot;
]
