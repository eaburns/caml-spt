(** Scatter plot datasets.

    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Geometry
open Drawing

let make_glyph_factory glyph_set () =
  (** [make_glyph_factory glyph_set ()] makes a glyph factory which
      returns a new glyph at each call. *)
  let next = ref 0 in
  let n = Array.length glyph_set in
    (fun () ->
       let g = glyph_set.(!next) in
	 next := (!next + 1) mod n;
	 g)


let default_glyph_factory =
  (** [default_glyph_factory] gets the default glyph factory
      builder. *)
  let default_glyph_set =
    [| Circle_glyph;
       Ring_glyph;
       Plus_glyph;
       Triangle_glyph;
       Box_glyph;
       Square_glyph;
       Cross_glyph;
    |]
  in make_glyph_factory default_glyph_set



class scatter_dataset glyph ?(color=black) ?(radius=0.012) ?name points =
  (** A scatter plot dataset. *)
object (self)
  inherit points_dataset ?name points

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
      draw_points ctx ~color radius glyph !pts

end

