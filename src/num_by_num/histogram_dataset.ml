(** Histograms show an approximate distribution given a set of
    floating point values.

    @author jtd7
    @since 2010-05-24
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

let bin_start ~bin_min ~bin_width index =
  (** value at the left edge of bin [i] in [h] *)
  bin_min +. (bin_width *. (float index))


let bin_end ~bin_min ~bin_width index =
  (** Value at the right edge of bin [i] in [h].
      This is not implemented interms of bin_start because (even when
      inlined) OCaml boxes the return value of bin_start. *)
  bin_min +. (bin_width *. (float (index + 1)))


let bucket ~bin_min ~bin_width value =
  (** the index of the bin that should contain [value].  Note that any
      value outside the range of representable integers, such as
      [infinity], may yield a garbage value, such as 0!  If you might pass
      such a value, test before calling!  (This routine is intended to be
      simple and fast.) *)
  truncate ((value -. bin_min) /. bin_width)


let get_bin_width bin_width ~min_value ~max_value count =
  (** [get_bin_width bin_width ~min_value ~max_value count] gets the
      width to use for each bin. *)
  match bin_width with
    | None ->
	let nbins = sqrt (float count) in
	  (* This is a common default bin number (used by Excel). *)
	  (max_value -. min_value) /. nbins
    | Some w -> w


let normalize_bins bins =
  (** [normalize_bins bins] normalizes the bins to sum to 1. *)
  let sum = Array.fold_left (+.) 0. bins in
  let n = Array.length bins in
    for i = 0 to n - 1 do
      bins.(i) <- bins.(i) /. sum;
    done


let make_bins ?(normalize=false) bin_width values =
  (** [make_bins ?normalize bin_width values] creates an array of
      bins. *)
  let min_value, max_value, count =
    Array.fold_left (fun (min, max, n) v ->
		       let min' = if v < min then v else min
		       and max' = if v > max then v else max
		       in min', max', n + 1)
      (infinity, neg_infinity, 0) values
  in
  let range = max_value -. min_value in
  let bin_width = get_bin_width bin_width ~min_value ~max_value count in
  let bin_count = max (truncate (ceil (range /. bin_width))) 1 in
  let ave_per_bin = (float count) /. (float bin_count) in
  let bin_min = min_value -. (bin_width /. ave_per_bin)  in
  let bin_max = bin_end ~bin_min ~bin_width (bin_count - 1) in
  let bins = Array.create (bin_count + 1) 0. in
  let max_weight = ref 0. in
    Array.iter
      (fun v ->
	 let bi = bucket ~bin_min ~bin_width v in
	 let c = bins.(bi) +. 1. in
	   bins.(bi) <- c;
	   if c > !max_weight then max_weight := c)
      values;
    if normalize then normalize_bins bins;
    !max_weight, bin_min, bin_max, bin_width, bins


class histogram_dataset
  dashes ?(normalize=false) ?(line_width=Length.Pt 1.) ?(bg_color=gray)
  ?bin_width ?name values =
  let max_weight, bin_min, bin_max, bin_width, bins =
    make_bins ~normalize bin_width values
  in
object(self)

  inherit dataset ?name ()

  val style = { line_color = black;
		line_dashes = dashes;
		line_width = line_width; }


  method dimensions =
    rectangle ~x_min:bin_min ~x_max:bin_max ~y_min:0. ~y_max:max_weight


  method mean_y_value _ =
    let s, n =
      Array.fold_left (fun (s, n) y -> s +. y, n + 1) (0., 0) bins
    in s /. (float n), n


  method residual ctx ~src ~dst = zero_rectangle


  method draw ctx ~src ~dst =
    let tr_rect = rectangle_transform ~src ~dst in
    let tr_pt = point_transform ~src ~dst in
      Array.iteri
	(fun index count ->
	   if count > 0.
	   then begin
	     let y_max = count
	     and x_min = bin_start ~bin_min ~bin_width index
	     and x_max = bin_end ~bin_min ~bin_width index in
	     let r = rectangle ~x_min ~x_max ~y_min:0. ~y_max in
	     let outline = [ point x_min 0.;
			     point x_min y_max;
			     point x_max y_max;
			     point x_max 0.;
			     point x_min 0.; ]
	     in
	       begin match clip_rectangle ~box:src ~r with
		 | Some r -> fill_rectangle ctx ~color:bg_color (tr_rect r)
		 | None -> ()
	       end;
	       draw_line ctx ~box:src ~tr:tr_pt ~style outline;
	   end)
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
      fill_rectangle ctx ~color:bg_color r;
      draw_line ctx ~style outline


  method legend_dimensions ctx =
    (ctx.units line_legend_length),
      (max ((ctx.units line_legend_length) /. 4.)
	 (ctx.units line_width))


  method avg_slope = nan
end


let histogram_dataset
    dashes ?normalize ?line_width ?bg_color ?bin_width ?name values =
  (** [histogram_dataset dashes ?normalize ?line_width ?bg_color
      ?bin_width ?name values] makes a histogram. *)
  new histogram_dataset dashes ?normalize ?line_width ?bg_color
    ?bin_width ?name values

(* EOF *)
