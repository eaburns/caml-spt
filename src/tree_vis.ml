(** A plot type for drawing a tree.

    @author eaburns
    @since 2010-06-23
*)

open Geometry
open Drawing
open Printf

type node = {
  color : color;
  succs : node array;
}

let rec max_depth ?(depth=0) n =
  (** [max_depth ?depth n] gets the max depth of the tree. *)
  (Array.fold_left
     (fun m n' -> max (max_depth ~depth:(depth+1) n') m)
     0 n.succs) + 1


(** {1 Tree vis plot} ****************************************)

class type plot_type =
object
  method draw : Drawing.context -> unit
end

class plot ?title draw_style root =
object

  inherit Spt.plot title

  method draw ctx =
    let w = ctx.units width and h = ctx.units height in
      draw_style ctx ~width:w ~height:h root

end

let plot ?title draw_style root = new plot ?title draw_style root
  (** [plot ?title draw_style tree] creates a new tree visualization
      plot with the given tree dataset. *)


(** {1 Drawing sunbursts} ****************************************)

module Sunburst = struct

  let slice_line_style = { default_line_style with line_width = Length.Pt 1. }

  let rec draw_tree ctx center ?(depth=0) ~r ~dr ~t ~dt node =
    (** [draw_tree ctx center ?depth ~r ~dr ~t ~dt node] draws a subtree
	in the pi-wedge between angles [t] and [t + dt].  The root node
	ends at radius [r] and subsequent nodes are [dr] wider than the
	root.  This draws "pie-slices" and uses the painter's algorithm
	to make each node appear as a sector of a circle. *)
    let nsuccs = float (Array.length node.succs) in
    let dt' = dt /. nsuccs and r' = r +. dr and depth' = depth + 1 in
      ignore (Array.fold_left
		(fun t s ->
		   draw_tree ctx center ~depth:depth' ~r:r' ~dr ~t ~dt:dt' s;
		   t +. dt')
		t node.succs);
      fill_slice ctx center ~radius:r ~theta:t ~dtheta:dt node.color
(*
      draw_slice ctx ~style:slice_line_style center ~radius:r
	~theta:t ~dtheta:dt black
*)


  let style ctx ~width ~height root =
    let cx = width /. 2. and cy = height /. 2. in
    let center = point cx cy in
    let max_radius = min cx cy in
    let dr = max_radius /. (float (max_depth root)) in
      draw_tree ctx center ~r:dr ~dr ~t:0. ~dt:two_pi root
end

