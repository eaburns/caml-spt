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
    text_size = Length.Pt 10.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


type label_x_location =
  | Label_xat
  | Label_before
  | Label_after


type label_y_location =
  | Label_yat
  | Label_above
  | Label_below


class label_dataset
  ?(style=default_style)
  ?(xloc=Label_xat) ?(yloc=Label_yat)
  ?(xoff=(Length.Pt 0.)) ?(yoff=(Length.Pt 0.))
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
    let x = pt'.x +. (ctx.units xoff) and y = pt'.y +. (ctx.units yoff) in
    let w, h = text_dimensions ctx ~style txt in
    let x' = match xloc with
      | Label_xat -> x
      | Label_before -> x -. (w /. 2.)
      | Label_after -> x +. (w /. 2.)
    and y' = match yloc with
      | Label_yat -> y
      | Label_above -> y -. (h /. 2.)
      | Label_below -> y +. (h /. 2.)
    in point x' y'


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


  method draw_legend _ ~x:_ ~y:_ = ()

  method legend_dimensions _ = 0., 0.

end
