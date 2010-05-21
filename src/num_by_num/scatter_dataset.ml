(** Scatter plot datasets.

    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Geometry
open Drawing


let default_radius = Length.Pt 4.
  (** The default radius of the scatter points. *)


class scatter_dataset
  glyph ?(color=black) ?(radius=default_radius) ?name points =
  (** A scatter plot dataset. *)
object (self)
  inherit points_dataset ?name points

  method residual ctx ~src ~dst =
    (** [residual ctx ~src ~dst] if we were to plot this right now
	with the given [dst] rectangle, how far out-of-bounds will we
	go in each direction. *)
    let tr = rectangle_transform ~src ~dst in
      Array.fold_left
	(fun r pt ->
	   if rectangle_contains src pt
	   then rectangle_max r (point_residual dst (tr pt) (ctx.units radius))
	   else r)
	zero_rectangle points


  method draw ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
    let pts = ref [] in
      for i = (Array.length points) - 1 downto 0 do
	let pt = points.(i) in
	  if rectangle_contains src pt then pts := (tr pt) :: !pts;
      done;
      draw_points ctx ~color radius glyph !pts

  method draw_legend ctx ~x ~y = draw_point ctx ~color radius glyph (point x y)

  method legend_dimensions ctx =
    let r2 = (ctx.units radius) *. 2. in r2, r2

  method avg_slope = rectangle_to_slope self#dimensions

end


let scatter_dataset glyph ?color ?radius ?name point_list =
  new scatter_dataset glyph ?color ?radius ?name point_list


let scatter_datasets ?(uses_color=false) ?radius name_by_point_list_list =
  let next_glyph = Factories.default_glyph_factory () in
    List.map (fun (name, point_list) -> scatter_dataset (next_glyph())
		?radius	~name point_list) name_by_point_list_list


(** {2 Scatter plot with error bars} ****************************************)


let scatter_errbar_dataset glyph ?color ?(radius=default_radius) ?name sets =
  (** [scatter_errbara_dataset glyph ?color ?radius ?name sets]
      creates a new composite dataset that is as scatter plot with
      error bars and optionally labels on each point. *)
  let pts, lbls, x_errs, y_errs =
    Array.fold_left (fun (pts, lbls, x_errs, y_errs) (vls, name) ->
		       let xs = Array.map (fun p -> p.x) vls
		       and ys = Array.map (fun p -> p.y) vls in
		       let mu_x, int_x = Statistics.mean_and_interval xs
		       and mu_y, int_y = Statistics.mean_and_interval ys in
		       let pt = point mu_x mu_y in
		       let lbls' = match name with
			 | Some txt -> (pt, txt) :: lbls
			 | None -> lbls
		       in
			 (pt :: pts,
			  lbls',
			  triple mu_x mu_y int_x :: x_errs,
			  triple mu_x mu_y int_y :: y_errs))
      ([], [], [], []) sets in
  let scatter = new scatter_dataset glyph ?color ~radius (Array.of_list pts)
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
  in
    new composite_dataset ?name [scatter; horiz_err; vert_err; labels;]


let scatter_errbar_datasets ?(uses_color=false) name_by_sets_list =
  let next_glyph = Factories.default_glyph_factory () in
  List.map (fun (name,sets) ->
	      scatter_errbar_dataset (next_glyph()) ?name sets)
    name_by_sets_list

(* EOF *)
