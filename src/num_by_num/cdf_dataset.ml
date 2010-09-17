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


let points_cdf_dataset dashes ?(normalize=true) ?(nsamples=100)
    ?line_width ?color ?name pts =
  let cdf_pts = cdf_of_points nsamples pts in
    if not normalize
    then Line_dataset.line_dataset dashes ?color ?line_width ?name cdf_pts
    else begin
      let sum = float_ref 0. in
	Array.iter (fun p -> sum <-- !!sum +. p.y) pts;
	let cdf_pts' = Array.map (fun p -> point p.x (p.y /. !!sum)) cdf_pts in
	 Line_dataset.line_dataset dashes ?color ?line_width ?name cdf_pts'
    end


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
