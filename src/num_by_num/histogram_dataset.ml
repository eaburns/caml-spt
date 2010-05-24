(**

    @author jtd7
    @since 2010-05-24
   Histogram dataset

*)

open Num_by_num_dataset
open Drawing
open Geometry

type bin =
  { lower_end : float;
    upper_end : float;
    mutable count : int;}

let f_compare a b =
  let v = a -. b in
    if v < 0. then ~-1 else if v > 0. then 1 else 0

let line_legend_length = Length.Cm 0.75
  (** The length of the line drawn in the legend. *)


let default_line = { default_line_style with line_width = Length.Pt 1. }
and default_bins = 10.

let fits bin value =
  (* checks to see if [value] fits into the current bin, increments
     count if it does and returns true, else false *)
  if (bin.lower_end <= value) && (value <= bin.upper_end)
  then (bin.count <- bin.count + 1; true)
  else false


let bin_from_bin prev_bin bin_width =
  assert (bin_width <> 0.);
  { lower_end = prev_bin.upper_end;
    upper_end = prev_bin.upper_end +. bin_width;
    count = 0; }


let bin_from_value value bin_width =
  let half = bin_width /. 2. in
    { lower_end = value -. half;
      upper_end = value +. half;
      count = 1;}


let bin_to_point bin =
  { x = bin.lower_end +. ((bin.upper_end -. bin.lower_end) /. 2.);
    y = float bin.count}


let make_bins ~min_value ~bin_width values =
    let first_bin = bin_from_value min_value bin_width in
      first_bin.count <- first_bin.count - 1;
      let c_bin,bins =
	Array.fold_left
	  (fun (current, accum) value ->
	     if fits current value
	     then (current,accum)
	     else (if bin_width = 0.
		   then (bin_from_value value bin_width, (current::accum))
		   else
		     (let bi =
			truncate (ceil ((value -. min_value) /. bin_width)) in
		      let center = ((float bi) *. bin_width +. min_value) in
			bin_from_value center bin_width, (current::accum))))
	  (first_bin, []) values in
	c_bin::bins

let make_width ~bin_width ~max_value ~min_value =
  match bin_width with
      None -> (ceil (max_value -. min_value) /.
		 (default_bins -. 1.))
    | Some w -> w

class histogram_dataset dashes ?(width=Length.Pt 1.) ?(color=black)
  ?bin_width ?name vals =
  let values = Array.sort compare vals ; vals in
  let min_value = values.(0)
  and max_value = values.((Array.length values) - 1) in
  let bin_width = make_width ~bin_width ~max_value ~min_value in
  let bins = make_bins min_value bin_width values in

object(self)

  inherit dataset ?name ()

  val style = { line_color = color;
		line_dashes = dashes;
		line_width = width; }

  method dimensions =
    let min_x = ref infinity
    and max_x = ref neg_infinity
    and max_y = ref 0 in
      List.iter (fun bucket ->
		   min_x := min !min_x bucket.lower_end;
		   max_x := max !max_x bucket.upper_end;
		   max_y := max !max_y bucket.count) bins;
      rectangle ~x_min:!min_x ~x_max:!max_x ~y_min:0. ~y_max:(float !max_y)

  method residual ctx ~src ~dst =
    rectangle ~x_min:(1.) ~x_max:(1.) ~y_min:0. ~y_max:1.

  method draw ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
    List.iter (fun bin ->
		 let lleft = { x = bin.lower_end; y = 0.; }
		 and uleft = { x = bin.lower_end; y = float bin.count; }
		 and uright = { x = bin.upper_end; y = float bin.count; }
		 and lright = { x = bin.upper_end; y = 0.} in
		   fill_polygon ctx ~box:dst ~color
		     (List.map tr [lleft; uleft; uright; lright;])) bins

  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]


  method legend_dimensions ctx =
    (ctx.units line_legend_length), (ctx.units width)


  method avg_slope = nan
end


let histogram_connected_dataset dashes glyph ?width ?bin_width ?(color = black)
    ?name values =
  Array.sort f_compare values;
  let min_value = values.(0)
  and max_value = values.((Array.length values) - 1) in
  let histogram = new histogram_dataset dashes ?name values in
  let bins = make_bins ~min_value
    ~bin_width:(make_width ~bin_width ~max_value ~min_value) values in
    new composite_dataset ?name
      [histogram;
       Line_dataset.line_points_dataset dashes glyph ?width ~color ?name
	 (Array.of_list (List.map bin_to_point bins));]

(* EOF *)
