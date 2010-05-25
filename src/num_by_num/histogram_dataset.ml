(**

   @author jtd7
   @since 2010-05-24
   Histogram dataset

*)

open Num_by_num_dataset
open Drawing
open Geometry


let f_compare a b =
  let v = a -. b in
    if v < 0. then ~-1 else if v > 0. then 1 else 0

let line_legend_length = Length.Cm 0.75
  (** The length of the line drawn in the legend. *)


let default_line = { default_line_style with line_width = Length.Pt 1. }
and default_bins = 10.


let bin_start ~min ~bin_width index =
  (** value at the left edge of bin [i] in [h] *)
  min +. (bin_width *. (float index))


let bin_end ~min ~bin_width index =
  (** Value at the right edge of bin [i] in [h].
      This is not implemented interms of bin_start because (even when
      inlined) OCaml boxes the return value of bin_start. *)
  min +. (bin_width *. (float (index + 1)))


let bucket ~min ~bin_width value =
  (** the index of the bin that should contain [value].  Note that any
      value outside the range of representable integers, such as
      [infinity], may yield a garbage value, such as 0!  If you might pass
      such a value, test before calling!  (This routine is intended to be
      simple and fast.) *)
  truncate ((value -. min) /. bin_width)


let make_bins ~min_value ~bin_width values =
  let range = values.((Array.length values) - 1) -. min_value in
  let bin_count = truncate (ceil (range /. bin_width)) in
  let bins = Array.create (bin_count + 1) 0 in
    Array.iter
      (fun ele ->
	 let bi = truncate ((ele -. min_value) /. bin_width) in
	   bins.(bi) <- bins.(bi) + 1) values;
    bins


let make_width ~bin_width ~max_value ~min_value =
  match bin_width with
      None -> (ceil (max_value -. min_value) /. (default_bins -. 1.))
    | Some w -> w


class histogram_dataset dashes ?(width=Length.Pt 1.) ?(color=gray)
  ?bin_width ?name vals =
  let values = Array.sort compare vals ; vals in
  let min_value = values.(0)
  and max_value = values.((Array.length values) - 1) in
  let bin_width = make_width ~bin_width ~max_value ~min_value in
  let bins = make_bins min_value bin_width values in

object(self)

  inherit dataset ?name ()

  val style = { line_color = black;
		line_dashes = dashes;
		line_width = width; }

  method dimensions =
    rectangle ~x_min:(min_value -. (bin_width /. 2.))
      ~x_max:(max_value +. (bin_width /. 2.)) ~y_min:0.
      ~y_max:(float (Array.fold_left max 0 bins))


  method residual ctx ~src ~dst = zero_rectangle


  method draw ctx ~src ~dst =
    let tr_rect = rectangle_transform ~src ~dst in
    let tr_pt = point_transform ~src ~dst in
      Array.iteri (fun index count ->
		     let y_max = float count
		     and x_min = min_value +. ((float index) -. 0.5) *. bin_width
		     and x_max = min_value +. ((float index) +. 0.5) *. bin_width
		     in
		     let r = rectangle ~x_min ~x_max ~y_min:0. ~y_max
		     in
		     let outline = [ point x_min 0.;
				     point x_min y_max;
				     point x_max y_max;
				     point x_max 0.;
				     point x_min 0.;];
		     in
		       draw_line ctx ~box:src ~tr:tr_pt ~style outline;
		       match clip_rectangle ~box:src ~r with
			 | Some r -> fill_rectangle ctx ~color (tr_rect r)
			 | None -> ())
	bins


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units line_legend_length) /. 2.
    and quarter_length = (ctx.units line_legend_length) /. 4. in
    let x_min = x -. half_length
    and x_max = x +. half_length
    and y_min = y -. quarter_length
    and y_max = y +. quarter_length in
    let r = rectangle ~x_min ~x_max ~y_min:0. ~y_max in
    let outline = [ point x_min y_min;
		    point x_min y_max;
		    point x_max y_max;
		    point x_max y_min;
		    point x_min y_min;] in
      draw_line ctx ~style outline;
      fill_rectangle ctx ~color r


  method legend_dimensions ctx =
    (ctx.units line_legend_length),
  (max ((ctx.units line_legend_length) /. 4.)
     (ctx.units width))


  method avg_slope = nan
end


(* EOF *)
