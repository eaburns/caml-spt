(** A label dataset allows arbitrary lables to be placed on the plot.

    @author eaburns
    @since 2010-04-29
*)

open Num_by_num_dataset
open Drawing
open Geometry

let default_style =
  (** The default style for labels. *)
  {
    text_font = "Palatino-Roman";
    text_size = 0.03;
    text_slant = Cairo.FONT_SLANT_NORMAL;
    text_weight = Cairo.FONT_WEIGHT_NORMAL;
    text_color = black;
  }

class label_dataset ?(style=default_style) ?name label_points =
  (** Add labels where the points are given data-coordinates. *)
object

  inherit dataset ?name ()

  method dimensions =
    let pts = Array.map fst label_points in
      points_rectangle pts

  method residual ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
      Array.fold_left
	(fun r (pt, txt) ->
	   if rectangle_contains src pt
	   then begin
	     let pt' = tr pt in
	     let w, h = text_dimensions ctx ~style txt in
	     let x_min = pt'.x -. (w /. 2.)
	     and x_max = pt'.x +. (w /. 2.)
	     and y_max = pt'.y -. (h /. 2.)
	     and y_min = pt'.y +. (h /. 2.) in
	     let dims = rectangle ~x_min ~x_max ~y_min ~y_max in
	     let residue = rectangle_residual dst dims
	     in rectangle_max r residue
	   end else r)
	zero_rectangle label_points


  method draw ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
      Array.iter (fun (pt, txt) ->
		    let pt' = tr pt
		    in draw_text ctx ~style pt'.x pt'.y txt)
	label_points
end
