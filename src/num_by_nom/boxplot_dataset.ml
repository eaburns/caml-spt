(** Box plots.

    @author eaburns
    @since 2010-05-21
*)

open Geometry
open Drawing

let minf a b = if (a:float) < b then a else b
let maxf a b = if (a:float) > b then a else b

let line_style = { default_line_style with line_width = Length.Pt 1. }


let draw_mean_line ctx style src tr ~x0 ~x1 ~mean =
  (** [draw_mean_line cxt style src tr ~x0 ~x1 ~mean] draws the mean
      line if it is not clipped.  *)
  let mean' = tr mean in
    if mean <= src.max && mean >= src.min
    then draw_line ctx ~style [ point x0 mean'; point x1 mean' ]


let draw_box ctx style src tr ~x0 ~x1 ~q1 ~q3 =
  (** [draw_box ctx style src ~x0 ~x1 ~q1 ~q3] draws the boxplot box
      with clipping. *)
  let q3' = tr q3 and q1' = tr q1 in
  let q3_clipped = q3 > src.max || q3 < src.min
  and q1_clipped = q1 > src.max || q1 < src.min in
  if not q3_clipped then draw_line ctx ~style [ point x0 q3'; point x1 q3'];
  if not q1_clipped then draw_line ctx ~style [ point x0 q1'; point x1 q1'];
  if q1 <= src.max && q3 >= src.min
  then begin
    let y0 = if q1 < src.min then tr src.min else q1'
    and y1 = if q3 > src.max then tr src.max else q3' in
      draw_line ctx ~style [ point x0 y0; point x0 y1 ];
      draw_line ctx ~style [ point x1 y0; point x1 y1 ];
  end


let fill_ci_box ctx color src tr ~x0 ~x1 ~lower ~upper =
  (** [fill_ci_box ctx color src tr ~x0 ~x1 ~lower ~upper] fills in
      the confidence interval box if it is not clipped. *)
  if lower <= src.max && upper > src.min
  then begin
    let y_min = if lower < src.min then tr src.min else tr lower in
    let y_max = if upper > src.max then tr src.max else tr upper in
    let r = rectangle ~x_min:x0 ~x_max:x1 ~y_min ~y_max in
      fill_rectangle ctx ~color r
  end


class boxplot_dataset ?(point_radius=Length.Pt 2.) name values =
  let outliers, data = Statistics.separate_outliers values in
  let mean, conf_interval = Statistics.mean_and_interval data in
object(self)

  inherit Num_by_nom_dataset.dataset name

  val conf_lower = mean -. conf_interval
  val conf_upper = mean +. conf_interval
  val mean = mean
  val q1 = Statistics.percentile 25. values
  val q2 = Statistics.percentile 50. values
  val q3 = Statistics.percentile 75. values
  val outliers = outliers
  val data = data

  method dimensions =
    let min, max =
      Array.fold_left (fun (min, max) v -> (minf min v), (maxf max v))
	(infinity, neg_infinity) data
    in
    let vs =
      Array.append
	[| mean; conf_lower; conf_upper; q1; q2; q3;

	   (* The following are the computations of the error bar cap
	      locations.  These need to be here because of floating
	      point rounding issues... *)
	   q1 -. (q1 -. min);
	   q3 +. (max -. q3);
	|]
	values
    in
      range
	~min:(Array.fold_left minf infinity vs)
	~max:(Array.fold_left maxf neg_infinity vs)


  method residual ctx ~src ~dst ~width ~x =
    let r = ctx.units point_radius in
    let tr = range_transform ~src ~dst in
      Array.fold_left
	(fun res v ->
	   if v >= src.min && v <= src.max
	   then
	     let v' = tr v in
	     let max = if v' +. r > dst.max then (v' +. r) -. dst.max else 0.
	     and min = if v' -. r < dst.min then dst.min -. (v' -. r) else 0.
	     in range_max (range min max) res
	   else res)
	(range 0. 0.) outliers


  method draw ctx ~src ~dst ~width ~x =
    let lwidth = ctx.units line_style.line_width in
    let tr = range_transform ~src ~dst in
    let center = x +. (width /. 2.) in
    let x0 = x +. lwidth and x1 = x +. (width -. lwidth) in
    let conf_min = center -. (width /. 16.) in
    let conf_max = center +. (width /. 16.) in
    let min, max =
      Array.fold_left (fun (min, max) v -> (minf min v), (maxf max v))
	(infinity, neg_infinity) data
    in
      draw_points ctx point_radius ~color:black Ring_glyph
	(Array.fold_left (fun a v ->
			    if v >= src.min && v <= src.max
			    then (point center (tr v)) :: a
			    else a)
	   [] outliers);
      (*
	fill_rectangle ctx ~color:(color ~r:0.7 ~g:0.7 ~b:0.7 ~a:1.)
	(rectangle ~x_min:conf_min ~x_max:conf_max
	~y_min:(tr conf_lower) ~y_max:(tr conf_upper));
	draw_rectangle ctx ~style:line_style
	(rectangle ~x_min:x0 ~x_max:x1 ~y_min:(tr q1) ~y_max:(tr q3));
	draw_line ctx ~style:line_style [ point x0 mean'; point x1 mean'; ];
      *)
      fill_ci_box ctx (color ~r:0.7 ~g:0.7 ~b:0.7 ~a:1.) src tr
	~x0:conf_min ~x1:conf_max ~lower:conf_lower ~upper:conf_upper;
      draw_mean_line ctx line_style src tr ~x0 ~x1 ~mean;
      draw_box ctx line_style src tr ~x0 ~x1 ~q1 ~q3;
      if q3 > src.min && q3 < src.max
      then (Errbar.draw_up ctx ~style:line_style ~src ~dst
	      ~x:center ~y:q3 ~mag:(max -. q3));
      Errbar.draw_down ctx ~style:line_style ~src ~dst
	~x:center ~y:q1 ~mag:(q1 -. min)

end

let boxplot_dataset ?point_radius name values =
  (** [boxplot_dataset ?point_radius name values] makes a boxplot dataset. *)
  new boxplot_dataset ?point_radius name values


let boxplot_datasets ?point_radius name_vl_list =
  (** [boxplot_datasets ?point_radius name_vl_list] makes a set of boxplot
      datasets. *)
  List.map
    (fun (name, values) -> new boxplot_dataset ?point_radius name values)
    name_vl_list
