(** Virtual numeric by numeric datasets.

    @author eaburns
    @since 2010-04-28
*)

open Geometry
open Drawing

class virtual dataset ?name () =
  (** [dataset name] is a dataset that is plottable on a numeric x and
      y axis. *)
object
  val name = (name : string option)
    (** The name of the dataset.  If there is no name then the dataset
	doesn't appear in the legend. *)

  method virtual dimensions : rectangle
    (** [dimensions] is the dimensions of this dataset in
	data-coordinates. *)

  method residual
    (_:context) ~(src:rectangle) ~(dst:rectangle) (_:int) = zero_rectangle
    (** [residual ctx ~src ~dst rank] get a rectangle containing the
	maximum amount the dataset will draw off of the destination
	rectangle in each direction. *)


  method virtual draw :
    context -> src:rectangle -> dst:rectangle -> int -> unit
    (** [draw ctx ~src ~dst rank] draws the data to the plot. *)


  method draw_legend_entry (_:context) ~(x:float) ~(y:float) (_:int) = y
    (** [draw_legend_entry ctx ~x ~y rank] draws the legend entry to
	the given location ([x] is the left-edge and [y] is top edge
	of the destination) and the result is the y-coordinate of the
	bottom edge of the entry that was just drawn. *)
end


(** {1 Points datasets} ****************************************)


class virtual points_dataset ?name points =
  (** A dataset composed of a set of points. *)
object
  inherit dataset ?name ()

  val points = (points : point array)
    (** The list of points. *)

  method dimensions = points_rectangle points
    (** [dimensions] gets the rectangle around the points. *)
end

(** {1 Composite datasets} ****************************************)


class composite_dataset ?name datasets =
  (** A dataset composed of a set of datasets. *)
object
  inherit dataset ?name ()

  val datasets = (datasets : dataset list)

  method dimensions =
    List.fold_left (fun r ds -> rectangle_extremes r ds#dimensions)
     (rectangle ~x_min:infinity ~x_max:neg_infinity
	~y_min:infinity ~y_max:neg_infinity) datasets

  method residual ctx ~src ~dst rank =
    List.fold_left (fun r ds ->
		      rectangle_extremes r (ds#residual ctx ~src ~dst rank))
      zero_rectangle datasets

  method draw ctx ~src ~dst rank =
    List.iter (fun ds -> ds#draw ctx ~src ~dst rank) datasets

end
