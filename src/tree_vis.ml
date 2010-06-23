(** A plot type for drawing a tree.

    @author eaburns
    @since 2010-06-23
*)

open Printf

class type plot_type =
object
  method draw : Drawing.context -> unit
end

class plot ?title tree_dataset =
object

  inherit Spt.plot title

  method draw ctx = tree_dataset#draw ctx ~width ~height

end

let plot ?title tree_dataset = new plot ?title tree_dataset
  (** [plot ?title tree_dataset] creates a new tree visualization plot
      with the given tree dataset. *)

include Tree_vis_dataset
include Tree_vis_sunburst

