(**

    @author jtd7
    @since 2010-05-24

   Box sets are groups of box plots.  Each box plot has its own name,
   and then a larger set to which it belongs.  Sets are drawn together,
   each box plot is labeled, and then the group is labeled beneath that.
*)

open Geometry
open Drawing

let minf a b = if (a:float) < b then a else b
let maxf a b = if (a:float) > b then a else b

let line_style = { default_line_style with line_width = Length.Pt 1. }


type boxplot_group =
  { box_plots : Boxplot_dataset.boxplot_dataset list;
    group : string }

class boxset_dataset ?(radius=Length.Pt 2.) boxplot_group =

object(self)

  inherit Num_by_nom_dataset.dataset boxplot_group.group

  method dimensions =
    let dims = List.map (fun bds -> bds#dimensions) boxplot_group.box_plots in
    let min = List.fold_left (fun accum value ->
				minf accum value.min) infinity dims
    and max = List.fold_left (fun accum value ->
				maxf accum value.min) neg_infinity dims in

      failwith "unimplemented"

  method residual ctx ~src ~dst ~width ~x =
    failwith "unimplemented"

  method draw ctx ~src ~dst ~width ~x =
    failwith "unimplemented"

end


(* EOF*)
