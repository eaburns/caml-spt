(** Numeric by nominal datasets.

    @author eaburns
    @since 2010-05-21
*)

open Drawing
open Geometry

class virtual dataset name =
  (** [dataset name] is a dataset that is plottable on a nominal x
      axis and a numeric y axis. *)
object

  val name = (name : string)
    (** The name of the dataset is what appears on the x-axis. *)

  method virtual dimensions : range
    (** [dimensions] gets the min and maximum value from the
	dataset. *)


  method x_label_height : context -> text_style -> float -> float =
    (** [x_label_height context style width] is the height of the
	label on thesrc/ x-axis. *)
    (fun ctx style width -> fixed_width_text_height ctx ~style width name)


  method draw_x_label :
    context -> x:float -> y:float -> text_style -> text_width:float -> unit =
    (** [draw_x_label context ~x ~y style ~text_width] draws the x-axis label to
	the proper location. *)
    (fun ctx ~x ~y style ~text_width ->
       draw_fixed_width_text ctx ~x ~y ~style ~width:text_width name)

  method virtual residual :
    context -> src:range -> dst:range -> width:float -> x:float -> range
    (** [residual ctx ~src ~dst width x] get a rectangle containing
	the maximum amount the dataset will draw off of the
	destination rectangle in each direction. *)

  method virtual draw :
    context -> src:range -> dst:range -> width:float -> x:float ->unit
    (** [draw ctx ~src ~dst width x] draws the dataset to the
	plot.  [x] is the left-hand-side x value. *)
end
