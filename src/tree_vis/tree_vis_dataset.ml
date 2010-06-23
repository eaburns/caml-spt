(** Shared dataset code for a tree plot.

    @author eaburns
    @since 2010-06-23
*)

open Drawing

type node = {
  color : color;
  succs : node array;
}


let rec max_depth ?(depth=0) n =
  (** [max_depth ?depth n] gets the max depth of the tree. *)
  (Array.fold_left
     (fun m n' -> max (max_depth ~depth:(depth+1) n') m)
     0 n.succs) + 1


class type tree_vis_dataset_type =
object
  method draw : Drawing.context -> width:Length.t -> height:Length.t -> unit
end
