(** Scatter plot datasets.

    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Geometry
open Drawing

let glyphs =
  (** The default glyphs for scatter plots. *)
  [| Circle_glyph;
     Ring_glyph;
     Plus_glyph;
     Triangle_glyph;
     Box_glyph;
     Square_glyph;
     Cross_glyph;
  |]

class scatter_dataset ?glyph ?(color=black) ?(radius=0.012) ?name points =
  (** A scatter plot dataset. *)
object (self)
  inherit points_dataset ?name points

  method private glyph rank = match glyph with
      (** [glyph rank] the glyph to use for this dataset. *)
    | None -> glyphs.(rank)
    | Some g -> g


  method residual ctx ~src ~dst _ =
    (** [residual ctx ~src ~dst rank] if we were to plot this right
	now with the given [dst] rectangle, how far out-of-bounds will
	we go in each direction. *)
    let tr = rectangle_transform ~src ~dst in
      Array.fold_left
	(fun r pt ->
	   if rectangle_contains src pt
	   then rectangle_max r (point_residual dst (tr pt) radius)
	   else r)
	zero_rectangle points


  method draw ctx ~src ~dst rank =
    let tr = rectangle_transform ~src ~dst in
    let pts = ref [] in
      for i = (Array.length points) - 1 downto 0 do
	let pt = points.(i) in
	  if rectangle_contains src pt then pts := (tr pt) :: !pts;
      done;
      draw_points ctx ~color radius (self#glyph rank) !pts

  method draw_legend_entry ctx ~x ~y rank = failwith "Unimplemented"

end

