(** Lines and lines with errorbars.

    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Drawing
open Geometry

let line_legend_length = Length.Cm 0.75
  (** The length of the line drawn in the legend. *)


class line_dataset dashes ?(width=Length.Pt 1.) ?(color=black) ?name points =
  (** A line plot dataset. *)
object (self)
  inherit points_dataset ?name points

  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = width;
    }

  method draw ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
    let pts = ref [] in
      for i = (Array.length points) - 1 downto 0 do
	pts := (tr points.(i)) :: !pts
      done;
      draw_line ctx ~box:dst ~style !pts


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]


  method legend_dimensions ctx =
    (ctx.units line_legend_length), (ctx.units width)

  method avg_slope =
    let pts = points in
      if Array.length pts < 2 then nan
      else
	(let accum = ref 0.
	 and count = (Array.length pts - 2) in
	   for i = 0 to count
	   do
	     (let pt1 = pts.(i)
	      and pt2 = pts.(i+1) in
	      let dy = pt2.y -. pt1.y
	      and dx = pt2.x -. pt1.x in
		accum := !accum +. (abs_float (dy /. dx)))
	   done;
	   !accum /. (float count))

end

let default_radius =
  Length.Pt ((Length.as_pt Scatter_dataset.default_radius) /. 2.)


let line_points_dataset dashes glyph ?(radius=default_radius)
    ?width ?color ?name points =
  (** [line_points_dataset dashes glyph ?radius ?width ?color ?name
      points] makes a dataset that is a line with glyphs at each
      point. *)
  new composite_dataset ?name
    [new line_dataset dashes ?width ?color ?name points;
     new Scatter_dataset.scatter_dataset glyph ~radius ?color ?name points;]


let line_dataset dashes ?width ?color ?name points =
  new line_dataset dashes ?width ?color ?name points


let line_datasets ?(uses_color=false) name_by_points_list =
  let next_dash = Factories.default_dash_factory () in
  List.map (fun (name, points) ->
	      line_dataset (next_dash ()) ?name points)
    name_by_points_list


let line_points_datasets ?(uses_color=false) name_by_points_list =
  let next_dash = Factories.default_dash_factory ()
  and next_glyph = Factories.default_glyph_factory () in
  List.map (fun (name, points) ->
	      line_points_dataset (next_dash ()) (next_glyph()) ?name points)
  name_by_points_list


(* EOF *)
