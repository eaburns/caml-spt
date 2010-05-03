(** Numeric by nominal plots.

    @author eaburns
    @since 2010-04-26
*)

open Geometry
open Drawing

let y_axis_padding = 0.01
  (** The amount of room to separate the y-axis from the data. *)

let x_axis_padding = 0.05
  (** The amount of room to separate the x-axis from the data. *)

(** {1 Numeric by nomeric plot} ****************************************)

class plot
  ?(label_style=Ml_plot.default_label_style)
  ?(legend_style=Ml_plot.default_legend_style)
  ?(tick_style=Ml_plot.default_tick_style)
  ?title ?ylabel ?y_min ?y_max datasets =
  (** [plot ?label_style ?legend_style ?tick_style ?title ?ylabel
      ?y_min ?y_max datasets] a plot that has a nominal x axis and a
      numeric y axis. *)
object (self)
  inherit Ml_plot.plot title

  val datasets = datasets

  method private src_y_range =
    (** [src_y_range] computes the range of the y axis. *)
    match y_min, y_max with
      | Some min, Some max -> range ~min ~max
      | _ ->
	  let min, max =
	    List.fold_left (fun (min, max) ds ->
			      let ds_min, ds_max = ds#dimensions in
			      let min' = if ds_min < min then ds_min else min
			      and max' = if ds_max > max then ds_max else max
			      in min', max')
	      (infinity, neg_infinity) datasets
	  in range ~min ~max


  method private yticks =
    (** [yticks] computes the location of the y-axis tick marks. *)
    Numeric_axis.tick_locations self#src_y_range


  method private x_axis_dimensions ctx box =
    (** [x_axis_dimensions ctx box] computes the x_min, x_max and width
	for each dataset to display its name on the x-axis. *)
    let x_max = box.x_max in
    let x_min =
      Numeric_axis.resize_for_y_axis ctx ~label_style ~tick_style
	~pad:Ml_plot.text_padding ~x_min:(box.x_min +. y_axis_padding)
	ylabel self#yticks
    in
    let n = List.length datasets in
    let width = if n > 0 then (x_max -. x_min) /. (float n) else 0. in
      range x_min x_max, width


  method private dst_y_range ctx box ~y_min ~y_max ~width =
    (** [dst_y_range ctx box ~y_min ~y_max ~width] get the range on the
	y-axis.  [width] is the amount of width afforded to each
	dataset on the x-axis. *)
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:tick_style txt) in
    let data_label_height =
      List.fold_left (fun m ds ->
			let h = ds#x_label_height ctx width in
			  if h > m then h else m)
	0. datasets
    in
      range
	(box.y_max -. data_label_height -. x_axis_padding)
	(box.y_min +. title_height +. Ml_plot.text_padding)



  method private draw_y_axis ctx ~box ~src ~dst =
    (** [draw_y_axis ctx ~src ~dst] draws the y-axis. *)
    Numeric_axis.draw_y_axis ctx
      ~tick_style ~label_style ~pad:Ml_plot.text_padding
      ~box ~src ~dst ylabel self#yticks


  method private draw_x_axis ctx ~y ~xrange ~width =
    (** [draw_x_axis ctx ~y ~xrange ~width] draws the x-axis. *)
    set_text_style ctx legend_style;
    ignore (List.fold_left
	      (fun x ds ->
		 ds#draw_x_label ctx ~x ~y ~width;
		 x +. width)
	      (xrange.min +. (width /. 2.)) datasets)


  method draw ctx box =
    (** [draw ctx box] draws the plot. *)
    let src = self#src_y_range in
    let xrange, width = self#x_axis_dimensions ctx box in
    let dst = self#dst_y_range ctx box ~y_min ~y_max ~width in
      begin match title with
	| None -> ()
	| Some t ->
	    let x = (box.x_max +. box.x_min) /. 2. and y = box.y_min in
	      draw_text_centered_below ~style:label_style ctx x y t
      end;
      self#draw_y_axis ctx ~box ~src ~dst;
      self#draw_x_axis ctx ~y:(dst.min +. x_axis_padding) ~xrange ~width
end


(** {1 Datasets} ****************************************)


class virtual dataset name =
  (** [dataset name] is a dataset that is plottable on a nominal x
      axis and a numeric y axis. *)
object

  val name = (name : string)
    (** The name of the dataset is what appears on the x-axis. *)

  method virtual dimensions : float * float
    (** [dimensions] gets the min and maximum value from the
	dataset. *)


  method x_label_height : context -> float -> float =
    (** [x_label_height context width] is the height of the label on the
	x-axis. *)
    (fun ctx width -> fixed_width_text_height ctx width name)


  method draw_x_label :
    context -> x:float -> y:float -> width:float -> unit =
    (** [draw_x_label context ~x ~y ~width] draws the x-axis label to
	the proper location. *)
    (fun ctx ~x ~y ~width -> draw_fixed_width_text ctx ~x ~y ~width name)

  method virtual residual :
    context -> src:range -> dst:range -> float -> int -> range
    (** [residual ctx ~src ~dst width rank] get a rectangle containing the
	maximum amount the dataset will draw off of the destination
	rectangle in each direction. *)

  method virtual draw :
    context -> src:range -> dst:range -> float -> int -> unit
    (** [draw ctx ~src ~dst width rank] draws the dataset to the
	plot. *)
end
