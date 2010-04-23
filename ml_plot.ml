(** The plot hierarchy.

    @author eaburns
    @since 2010-04-23
*)

open Geometry
open Drawing

(** {1 Plots} ****************************************)

class virtual plot title =
  (** [plot title] makes a plot with the given title. *)
object
  val title = (title : string option)

  method virtual draw : context -> unit
    (** [draw ctx] displays the plot to the given drawing
	context. *)
end


and num_by_num_plot ?text_style ~title ~xlabel ~ylabel ?dims datasets =
  (** [num_by_num_plot ~title ~xlabel ~ylabel ?dims datasets] a plot
      that has a numeric x and y axis. *)
object (self)
  inherit plot title

  val datasets = (datasets : num_by_num_dataset list)
  val text_style = (text_style : text_style option)
  val xlabel = (xlabel : string option)
  val ylabel = (ylabel : string option)

  val text_padding = 0.01
    (** Padding around text *)

  val dims = match dims with
    | None -> failwith "Automatic dimensions is currently unimplemented"
    | Some rect -> rect


  method private text_height ctx = function
      (** [text_height ctx txt] gets the height of an optional piece
	  of text. *)
    | None -> 0.
    | Some txt -> snd (text_dimensions ctx ?style:text_style txt)


  method draw ctx =
(*
    let title_height = self#text_height ctx title
    and xlabel_height = self#text_height ctx xlabel
    and ylabel_height = self#text_height ctx ylabel in
    let dst =
      rectangle ~x_min:(ylabel_height +. text_padding) ~x_max:1.
	~y_min:(1. -. (xlabel_height +. text_padding))
	~y_max:(title_height +. text_padding)
    in
*)
      ()

end


and num_by_nom_plot ~title ~ylabel datasets =
  (** [num_by_nom_plot ~title ~ylabel datasets] a plot that has a nominal x
      axis and a numeric y axis. *)
object
  inherit plot title

  val datasets = (datasets : num_by_nom_dataset list)
  val ylabel = (ylabel : string option)

  method draw _ = failwith "Unimplemented"
end


(** {1 Datasets} ****************************************)

and virtual num_by_num_dataset name =
  (** [num_by_num_dataset name] is a dataset that is plottable on a
      numeric x and y axis. *)
object
  val name = (name : string option)
    (** The name of the dataset.  If there is no name then the dataset
	doesn't appear in the legend. *)

  method virtual dimensions : rectangle
    (** [dimensions] is the dimensions of this dataset in
	data-coordinates. *)

  method virtual draw :
    context -> src:rectangle -> dst:rectangle -> int -> unit
    (** [draw ctx ~src ~dst rank] draws the data to the plot given
	[src], the data coordinate system and [dst] the destination
	coordinate system and [rank] the number of datasets that were
	plotted before this one. *)

  method virtual draw_legend_entry :
    context -> x:float -> y:float -> float
    (** [draw_legend_entry ctx ~x ~y] draws the legend entry to the
	given location ([x] is the left-edge and [y] is top edge of the
	destination) and the result is the y-coordinate of the bottom edge
	of the entry that was just drawn. *)
end

and virtual num_by_nom_dataset name =
  (** [num_by_nom_dataset name] is a dataset that is plottable on a
      nominal x axis and a numeric y axis. *)
object

  val name = (name : string)
    (** The name of the dataset is what appears on the x-axis. *)

  method virtual x_label_height : float -> float
    (** [x_label_height width] is the height of the label on the
	x-axis. *)

  method virtual draw_x_label :
    context -> x:float -> y:float -> width:float -> unit
    (** [draw_x_label context ~x ~y ~width] draws the x-axis label to
	the proper location. *)

  method virtual draw :
    context -> src:float -> dst:float -> width:float -> int -> unit
    (** [draw ctx src dst width rank] draws the dataset to the plot. *)

end
