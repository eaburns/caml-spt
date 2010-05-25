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
    let tr = point_transform ~src ~dst in
(*
    let pts = ref [] in
      for i = (Array.length points) - 1 downto 0 do
	pts := (tr points.(i)) :: !pts
      done;
*)
      draw_line ctx ~box:src ~tr ~style (Array.to_list points)


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
    if uses_color
    then (let next_color = Factories.default_color_factory () in
	    List.map (fun (name, points) ->
			line_dataset (next_dash ()) ~color:(next_color())
			  ?name points) name_by_points_list)
    else
      List.map (fun (name, points) ->
		  line_dataset (next_dash ()) ?name points)
	name_by_points_list


let line_points_datasets ?(uses_color=false) name_by_points_list =
  let next_dash = Factories.default_dash_factory ()
  and next_glyph = Factories.default_glyph_factory () in
  List.map (fun (name, points) ->
	      line_points_dataset (next_dash ()) (next_glyph()) ?name points)
  name_by_points_list


let scatter_errbar_lines_dataset glyph dash ?color ?(radius=default_radius)
    ?name sets =
  let pts, lbls, x_errs, y_errs =
    Array.fold_left (fun (pts, lbls, x_errs, y_errs) (vls, name) ->
		       let xs = Array.map (fun p -> p.x) vls
		       and ys = Array.map (fun p -> p.y) vls in
		       let mu_x, int_x = Statistics.mean_and_interval xs
		       and mu_y, int_y = Statistics.mean_and_interval ys in
		       let pt = point mu_x mu_y in
			 Printf.eprintf "%f x %f\n" mu_x mu_y;
		       let lbls' = match name with
			 | Some txt -> (pt, txt) :: lbls
			 | None -> lbls
		       in
			 (pt :: pts,
			  lbls',
			  triple mu_x mu_y int_x :: x_errs,
			  triple mu_x mu_y int_y :: y_errs))
      ([], [], [], []) sets in
  let scatter = new Scatter_dataset.scatter_dataset
    glyph ?color ~radius (Array.of_list pts)
  and labels =
    new Label_dataset.label_dataset
      ~yoff:(Length.Pt ~-.(Length.as_pt radius)) ~xoff:radius
      ~xloc:Label_dataset.Label_after
      ~yloc:Label_dataset.Label_above
      (Array.of_list lbls)
  and horiz_err =
    new Errbar_dataset.horizontal_errbar_dataset (Array.of_list x_errs)
  and vert_err =
    new Errbar_dataset.vertical_errbar_dataset (Array.of_list y_errs)
  and line = line_dataset dash ?color ?name (Array.of_list pts)
  in
    new composite_dataset ?name [scatter; line; horiz_err; vert_err; labels;]



let scatter_errbar_lines_datasets ?(uses_color=false) name_by_sets_list =
  let next_glyph = Factories.default_glyph_factory ()
  and next_dash = Factories.default_dash_factory () in
    if uses_color
    then (let next_color = Factories.default_color_factory () in
	    List.map (fun (name,sets) ->
			scatter_errbar_lines_dataset (next_glyph())
			  (next_dash()) ~color:(next_color()) ~name sets)
	      name_by_sets_list)
    else
      List.map (fun (name,sets) ->
		  scatter_errbar_lines_dataset (next_glyph()) (next_dash())
		    ~name sets)	name_by_sets_list

(* EOF *)
