(** Box plots.

    @author eaburns
    @since 2010-05-21
*)

open Geometry
open Drawing


class boxplot_dataset ?(point_radius=Length.Pt 2.) name values =
object(self)

  inherit Num_by_nom_dataset.dataset name

  val box = Boxplot.create ~point_radius values

  method dimensions = Boxplot.dimensions box


  method residual ctx ~src ~dst ~width ~x =
    Boxplot.residual ctx ~src ~dst ~width ~x box


  method draw ctx ~src ~dst ~width ~x =
    Boxplot.draw ctx ~src ~dst ~width ~x:(x +. (width /. 2.)) box

end

let boxplot_dataset ?point_radius name values =
  (** [boxplot_dataset ?point_radius name values] makes a boxplot dataset. *)
  new boxplot_dataset ?point_radius name values


let boxplot_datasets ?point_radius name_vl_list =
  (** [boxplot_datasets ?point_radius name_vl_list] makes a set of boxplot
      datasets. *)
  List.map
    (fun (name, values) -> new boxplot_dataset ?point_radius name values)
    name_vl_list
