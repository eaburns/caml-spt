(** Creating and drawing boxplots.

    @author eaburns
    @since 2010-06-09
*)

open Drawing
open Geometry

type t = {
  stats : stats;
  outliers : float array;
  color : Drawing.color;
  glyph : Drawing.glyph;
  point_radius : Length.t;
}

and stats = {
  q1 : float;
  q2 : float;
  q3 : float;
  upper_extreme : float;
  lower_extreme : float;
  mean : float;
  conf_upper : float;
  conf_lower : float;
}

let box_line_style = { default_line_style with line_width = Length.Pt 1. }

let minf a b = if (a:float) < b then a else b
let maxf a b = if (a:float) > b then a else b

let separate_outliers ~lower ~upper vls =
  (** [separate_outliers ~lower ~upper vls] separates the outliers
      that are outside the upper and lower fence values. *)
  let out_lst, lower_extreme, upper_extreme =
    Array.fold_left
      (fun (os, l, u) v ->
	 if v > upper || v < lower
	 then v :: os, l, u
	 else begin
	   let u' = if v > u then v else u
	   and l' = if v < l then v else l
	   in os, l', u'
	 end)
      ([], infinity, neg_infinity) vls
  in Array.of_list out_lst, lower_extreme, upper_extreme


let create
    ?(color=black) ?(glyph=Ring_glyph) ?(point_radius=Length.Pt 2.) values =
  (** [create ?color ?glyph ?point_radius values] makes a boxplot for
      the given values.

      Computed according to Tukey, "Exploratory Data Analysis" with
      the addition of 95% confidence intervals. *)
  let mean, conf_interval = Statistics.mean_and_interval values in
  let q1 = Statistics.percentile 25. values
  and q3 = Statistics.percentile 75. values in
  let step = 1.5 *. (q3 -. q1) in
  let upper_fence = q3 +. step and lower_fence = q1 -. step in
  let outliers, lower_extreme, upper_extreme =
    separate_outliers ~lower:lower_fence ~upper:upper_fence values
  in
    {
      stats = {
	q1 = q1;
	q2 = Statistics.percentile 50. values;
	q3 = q3;
	upper_extreme = upper_extreme;
	lower_extreme = lower_extreme;
	mean = mean;
	conf_upper = mean +. conf_interval;
	conf_lower = mean -. conf_interval;
      };
      outliers = outliers;
      color = color;
      glyph = glyph;
      point_radius = point_radius;
    }


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


let dimensions box =
  (** [dimensions box] computes the dimensions of the box. *)
  let vs =
    Array.append
      [| box.stats.upper_extreme; box.stats.lower_extreme; |]
      box.outliers
  in
    range
      ~min:(Array.fold_left minf infinity vs)
      ~max:(Array.fold_left maxf neg_infinity vs)


let residual ctx ~src ~dst ~width ~x box =
  (** [residual ctx ~src ~dst ~width ~x box] computes the residual. *)
  let r = ctx.units box.point_radius in
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
      (range 0. 0.) box.outliers


let draw ctx ~src ~dst ~width ~x box =
  (** [draw ctx ~src ~dst ~width ~x box] draws the boxplot centered at
      [x] with the given [width].  [src] and [dst] are the source and
      destination ranges for the y-axis. *)
  let tr = range_transform ~src ~dst in
  let lwidth = ctx.units box_line_style.line_width in
  let x0 = x -. (width /. 2.) +. lwidth
  and x1 = x +. (width /. 2.) -. lwidth in
  let color = box.color and radius = box.point_radius and glyph = box.glyph in
  let outliers = box.outliers in
  let q1 = box.stats.q1 and q3 = box.stats.q3 in
  let lower = box.stats.lower_extreme and upper = box.stats.upper_extreme in
    for i = 0 to (Array.length box.outliers) - 1 do
      let y = outliers.(i) in
	if y <= src.max && y >= src.min
	then draw_point ctx ~color radius glyph (point x (tr y));
    done;
    fill_ci_box ctx { color with a = (color.a /. 2.) } src tr
      ~x0:(x -. (width /. 16.)) ~x1:(x +. (width /. 16.))
      ~lower:box.stats.conf_lower
      ~upper:box.stats.conf_upper;
    draw_mean_line ctx box_line_style src tr ~x0 ~x1 ~mean:box.stats.mean;
    draw_box ctx box_line_style src tr ~x0 ~x1
      ~q1:box.stats.q1 ~q3:box.stats.q3;
    Errbar.draw_up ctx ~style:box_line_style ?cap_size:None
      ~src ~dst ~x ~y:q3 ~mag:(upper -. q3);
    Errbar.draw_down ctx ~style:box_line_style ?cap_size:None
      ~src ~dst ~x ~y:q1 ~mag:(q1 -. lower)
