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
  let glyph_set =
    [| Circle_glyph;
       Ring_glyph;
       Plus_glyph;
       Triangle_glyph;
       Box_glyph;
       Square_glyph;
       Cross_glyph;
    |]
  in make_glyph_factory glyph_set


let numbered_glyph_factory =
  (** [numbered_glyph_factory] gets a glyph factory builder that
      returns numbers as the glyphs. *)
  let glyph_set =
    [| Char_glyph '0';
       Char_glyph '1';
       Char_glyph '2';
       Char_glyph '3';
       Char_glyph '4';
       Char_glyph '5';
       Char_glyph '6';
       Char_glyph '7';
       Char_glyph '8';
       Char_glyph '9';
    |]
  in make_glyph_factory glyph_set


class scatter_dataset glyph ?(color=black) ?(radius=0.012) ?name points =
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
	   then rectangle_max r (point_residual dst (tr pt) radius)
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

end


(** {2 Scatter plot with error bars} ****************************************)

class scatter_errbar_dataset glyph ?color ?radius ?name point_sets =
  (** A scatter bar dataset with errorbars.  The [point_set] is an
      array of points arrays. *)
  let pts, x_errs, y_errs =
    Array.fold_left (fun (pts, x_errs, y_errs) vls ->
		       let xs = Array.map (fun p -> p.x) vls
		       and ys = Array.map (fun p -> p.y) vls in
		       let mu_x, int_x = Statistics.mean_and_interval xs
		       and mu_y, int_y = Statistics.mean_and_interval ys in
			 (point mu_x mu_y :: pts,
			  triple mu_x mu_y int_x :: x_errs,
			  triple mu_x mu_y int_y :: y_errs))
      ([], [], []) point_sets in
  let scatter = new scatter_dataset glyph ?color ?radius (Array.of_list pts)
  and horiz_err =
    new Errbar_dataset.horizontal_errbar_dataset (Array.of_list x_errs)
  and vert_err =
    new Errbar_dataset.vertical_errbar_dataset (Array.of_list y_errs) in
object (self)
  inherit composite_dataset ?name [scatter; horiz_err; vert_err]
end
