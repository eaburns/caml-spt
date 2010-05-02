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


type label_location =
  | Label_at
  | Label_above
  | Label_below
  | Label_before
  | Label_after

class label_dataset
  ?(style=default_style) ?(loc=Label_at) ?(xoff=0.) ?(yoff=0.)
  ?name label_points =
  (** Add labels where the points are given data-coordinates.  [xoff]
      and [yoff] are plot-coordinate offsets to apply to the label before
      drawing it at its given location.  This can be used to place the
      label outside of the radius of a scatter point. *)
object (self)

  inherit dataset ?name ()

  method dimensions =
    let pts = Array.map fst label_points in
      points_rectangle pts


  method private position ctx pt' txt =
    (** [position ctx pt' txt] position the label in the plot
	coordinate system. *)
    let x = pt'.x +. xoff and y = pt'.y +. yoff in
    let w, h = text_dimensions ctx ~style txt in
      match loc with
	| Label_at -> point x y
	| Label_above -> point x (y -. (h /. 2.))
	| Label_below -> point x (y +. (h /. 2.))
	| Label_before -> point (x -. (w /. 2.)) y
	| Label_after -> point (x +. (w /. 2.)) y


  method residual ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
      Array.fold_left
	(fun r (pt, txt) ->
	   if rectangle_contains src pt
	   then begin
	     let pt' = self#position ctx (tr pt) txt in
	     let dims = text_rectangle ctx ~style ~pt:pt' txt in
	     let residue = rectangle_residual dst dims
	     in rectangle_max r residue
	   end else r)
	zero_rectangle label_points


  method draw ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
      Array.iter (fun (pt, txt) ->
		    let pt' = self#position ctx (tr pt) txt in
		      draw_text ctx ~style pt'.x pt'.y txt)
	label_points
end
