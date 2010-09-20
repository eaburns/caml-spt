(**

    @author jtd7
    @since 2010-05-24
*)

open Num_by_num_dataset
open Drawing
open Geometry
open Statistics

let f_compare a b =
  if (a:float) < b then -1 else if a = b then 0 else 1


(*
let cdf_of_points nsamples pts =
  (** [cdf_of_points nsamples pts] computes a set of points for a CDF
      from the set of 'run-length encoded' data points ([pts]). *)
  let rec sum_wt sum i x =
    if i >= (Array.length pts) || pts.(i).x > x
    then sum
    else sum_wt (pts.(i).y +. sum) (i + 1) x
  in
  let min, max = Statistics.min_and_max (fun p -> p.x) pts in
  let delta = (max -. min) /. (float (nsamples - 1)) in
    Array.sort (fun a b -> f_compare a.x b.x) pts;
    Array.init nsamples (fun i ->
			   let x = min +. (float i *. delta) in
			     assert (x <= max);
			     point x (sum_wt 0. 0 x))
*)


let cdf_samples normalize xmin xmax nsamples pts =
  (** [cdf_samples normalize xmin xmax nsamples pts] computes the
      samples for a CDF given a range of x values.  [pts] is assumed
      to be sorted. *)
  let rec sample delta pts pt x samples i accum =
    if i < nsamples
    then begin
      if pt >= (Array.length pts)
      then
	let p = point x accum in
	  sample delta pts pt (x +. delta) (p :: samples) (i + 1) accum
      else begin
	let p = pts.(pt) in
	let px = p.x and py = p.y in
	  if px > x
	  then
	    let p = point x accum in
	      sample delta pts pt (x +. delta) (p :: samples) (i + 1) accum
	  else sample delta pts (pt + 1) x samples i (accum +. py)
      end
    end else samples
  in
  let delta = (xmax -. xmin) /. (float (nsamples - 1)) in
  let samples = sample delta pts 0 xmin [] 0 0. in
    if normalize
    then match samples with
      | { y = y } :: _ -> List.map (fun p -> { p with y = p.y /. y }) samples
      | [] -> []
    else samples


class cdf_dataset
  dashes ?(nsamples=100) ?(normalize=false) ?(line_width=Length.Pt 1.)
  ?(color=black) ?name pts =

  let _ = Array.sort (fun a b -> f_compare a.x b.x) pts in

  let x_min, x_max, y_max =
    let xmin = float_ref infinity in
    let xmax = float_ref neg_infinity in
    let ysum = float_ref 0. in
      Array.iter (fun p ->
		    let x = p.x and y = p.y in
		      if x < !!xmin then xmin <-- x;
		      if x > !!xmax then xmax <-- x;
		      ysum <-- !!ysum +. y)
	pts;
      !!xmin, !!xmax, if normalize then 1. else !!ysum
  in

object(self)

  inherit dataset ?name ()

  val style = { line_color = color;
		line_dashes = dashes;
		line_width = line_width; }


  method dimensions = rectangle ~x_min ~x_max ~y_min:0. ~y_max


  method mean_y_value src =
    let pts = cdf_samples normalize src.x_min src.x_max nsamples pts in
    let s = List.fold_left (fun s p -> s +. p.y) 0. pts
    in s /. (float nsamples), nsamples


  method residual ctx ~src ~dst = zero_rectangle


  method draw ctx ~src ~dst =
    let pts = cdf_samples normalize src.x_min src.x_max nsamples pts in
    let tr = point_transform ~src ~dst in
      draw_line ctx ~box:src ~tr ~style pts


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units Line_dataset.line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]


  method legend_dimensions ctx =
    (ctx.units Line_dataset.line_legend_length), (ctx.units line_width)


  method avg_slope = nan

end

let points_cdf_dataset dashes ?normalize ?nsamples ?line_width ?color
    ?name pts =
  new cdf_dataset dashes ?normalize ?nsamples ?line_width ?color ?name pts


let points_cdf_datasets ?normalize ?nsamples ?(color=false)
    name_by_points_list =
  let next_dash = Factories.default_dash_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name, points) ->
		points_cdf_dataset ?normalize ?nsamples ~color:(next_color ())
		  (next_dash ()) ~name points)
      name_by_points_list


let values_cdf_dataset dashes ?normalize ?nsamples ?line_width
    ?color ?name vls =
  let pts = Array.map (fun x -> point x 1.) vls in
    points_cdf_dataset ?normalize ?nsamples dashes ?line_width ?color ?name pts


let values_cdf_datasets ?normalize ?nsamples ?(color=false)
    name_by_values_list =
  let next_dash = Factories.default_dash_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name, values) ->
		values_cdf_dataset ?normalize ?nsamples ~color:(next_color ())
		  (next_dash ()) ~name values)
      name_by_values_list


let cdf_dataset = values_cdf_dataset

let cdf_datasets = values_cdf_datasets


(* EOF *)
