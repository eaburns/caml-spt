(**

    @author jtd7
    @since 2010-05-24
*)

open Num_by_num_dataset
open Drawing
open Geometry

let line_legend_length = Length.Cm 0.75
  (** The length of the line drawn in the legend. *)


let f_compare a b =
  let v = a -. b in
    if v < 0. then ~-1 else if v > 0. then 1 else 0


class cdf_dataset dashes ?(width=Length.Pt 1.) ?(color=black) ?name vals =
  (** A line plot dataset. *)

  let values = Array.sort f_compare vals; vals in

  let points =
    (** The points that form the CDF.  This is computed now because we
	need it to compute the mean_y_value. *)
    let size = float (Array.length values) in
      Array.init (truncate size)
	(fun i -> point values.(i) ((float i) /. size))
  in

object (self)

  inherit points_dataset ?name points


  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = width;
    }


  method dimensions =
    rectangle ~x_min:(values.(0)) ~x_max:(values.((Array.length values) - 1))
      ~y_min:0. ~y_max:1.


  method draw ctx ~src ~dst =
    let size = float (Array.length values) in
    let points = (Array.init (truncate size)
		    (fun i ->
		       point values.(i) ((float i) /. size))) in
    let tr = point_transform ~src ~dst in
      draw_line ctx ~box:dst ~style
	(Array.to_list (Array.map tr points))


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]


  method legend_dimensions ctx =
    (ctx.units line_legend_length), (ctx.units width)

  method avg_slope =
    if Array.length points < 2 then nan
    else
      (let accum = ref 0.
       and count = (Array.length points) - 2 in
	 for i = 0 to count
	 do
	   (let pt1 = points.(i)
	    and pt2 = points.(i+1) in
	    let dy = pt2.y -. pt1.y
	    and dx = pt2.x -. pt1.x in
	      accum := !accum +. (abs_float (dy /. dx)))
	 done;
	 !accum /. (float count))

end



let cdf_dataset dashes ?width ?color ?name points =
  new cdf_dataset dashes ?width ?color ?name points

let cdf_datasets ?(uses_color=false) name_by_values_list =
  let next_dash = Factories.default_dash_factory () in
    if uses_color
    then (let next_color = Factories.default_color_factory () in
	    List.map (fun (name, values) ->
			cdf_dataset (next_dash ()) ~color:(next_color())
			  ~name values) name_by_values_list)
    else
      List.map (fun (name, values) ->
		  cdf_dataset (next_dash ()) ~name values)
	name_by_values_list


(* EOF *)
