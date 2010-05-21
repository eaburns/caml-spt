(** Box plots.

    @author eaburns
    @since 2010-05-21
*)

open Geometry
open Drawing

let minf a b = if (a:float) < b then a else b
let maxf a b = if (a:float) > b then a else b

let line_style = { default_line_style with line_width = Length.Pt 1. }

class boxplot_dataset ?(radius=Length.Pt 2.) name values =
  let mean, conf_interval = Statistics.mean_and_interval values in
  let upper, lower = Statistics.upper_and_lower_fence values in
object(self)

  inherit Num_by_nom_dataset.dataset name

  val conf_lower = mean -. conf_interval
  val conf_upper = mean +. conf_interval
  val mean = mean
  val q1 = Statistics.percentile 25. values
  val q2 = Statistics.percentile 50. values
  val q3 = Statistics.percentile 75. values
  val upper = upper
  val lower = lower

  method dimensions =
    let vs =
      Array.append
	[| mean; conf_lower; conf_upper; q1; q2; q3; upper; lower; |]
	values
    in
      range
	~min:(Array.fold_left minf infinity vs)
	~max:(Array.fold_left maxf neg_infinity vs)


  method residual _ ~src:_ ~dst:_ ~width:_ ~x:_ = range 0. 0.


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst in
    let center = x +. (width /. 2.) in
    let quarter_min = center -. (width /. 4.) in
    let quarter_max = center +. (width /. 4.) in
    let eighth_min = center -. (width /. 8.) in
    let eighth_max = center +. (width /. 8.) in
    let outliers, data =
      Array.fold_left (fun (os, ds) v ->
			 if v > upper || v < lower
			 then v :: os, ds
			 else os, v :: ds)
	([], []) values
    in
    let min, max =
      List.fold_left (fun (min, max) v -> (minf min v), (maxf max v))
	(infinity, neg_infinity) data
    in
    let mean' = tr mean in
      draw_points ctx radius Ring_glyph
	(List.map (fun v -> point center (tr v)) outliers);
      draw_rectangle ctx ~style:line_style
	(rectangle ~x_min:quarter_min ~x_max:quarter_max
	   ~y_min:(tr q1) ~y_max:(tr q3));
      draw_line ctx ~style:line_style
	[ point quarter_min mean'; point quarter_max mean'; ];
      Errbar.draw_up ctx ~style:line_style ~src ~dst
	~x:center ~y:q3 ~mag:(max -. q3);
      Errbar.draw_down ctx ~style:line_style ~src ~dst
	~x:center ~y:q1 ~mag:(q1 -. min);
      fill_rectangle ctx
	(rectangle ~x_min:eighth_min ~x_max:eighth_max
	   ~y_min:(tr conf_lower) ~y_max:(tr conf_upper));

end
