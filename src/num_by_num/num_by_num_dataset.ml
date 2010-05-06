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

  method name = name
    (** [name] gets the (optional) name of the dataset. *)


  method residual
    (_:context) ~(src:rectangle) ~(dst:rectangle) = zero_rectangle
    (** [residual ctx ~src ~dst] get a rectangle containing the
	maximum amount the dataset will draw off of the destination
	rectangle in each direction. *)


  method virtual draw_legend : context -> x:float -> y:float -> unit
    (** [draw_legend ctx ~x ~y] draws the legend entry centered at the
	given location. *)


  method virtual legend_dimensions : context -> float * float
    (** [legend_dimensions ctx] gets the dimensions of the legend
	icon. *)


  method virtual draw :
    context -> src:rectangle -> dst:rectangle -> unit
    (** [draw ctx ~src ~dst] draws the data to the plot. *)


  method virtual dimensions : rectangle
    (** [dimensions] is the dimensions of this dataset in
	data-coordinates. *)

  method virtual avg_slope : float
    (** [avg_slope} returns the average rate of change across an
	entire num by num dataset.  Used for setting default axis skew
	(avg 45 slope for all elements of the plot) *)

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

  method residual ctx ~src ~dst =
    List.fold_left (fun r ds ->
		      rectangle_max r (ds#residual ctx ~src ~dst))
      zero_rectangle datasets

  method draw ctx ~src ~dst =
    List.iter (fun ds -> ds#draw ctx ~src ~dst) datasets


  method draw_legend ctx ~x ~y =
    List.iter (fun ds -> ds#draw_legend ctx ~x ~y) datasets

  method legend_dimensions ctx =
    List.fold_left (fun (w, h) ds ->
		      let width, height = ds#legend_dimensions ctx in
		      let w' = if width > w then width else w
		      and h' = if height > h then height else h
		      in w', h')
      (0., 0.) datasets

  method avg_slope =
    (List.fold_left (fun accum ds ->
		       let ds_slope = ds#avg_slope in
			 match classify_float ds_slope with
			   | FP_nan -> accum
			   | _ -> accum +. ds_slope)
       0. datasets) /. (float (List.length datasets))

end
