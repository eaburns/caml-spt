(** The plot hierarchy.

    @author eaburns
    @since 2010-04-23
*)

open Geometry
open Drawing

let default_tick_style =
  (** The default style for the text associated with tick marks on a
      numeric axis. *)
  {
    text_font = "Palatino-Roman";
    text_size = 0.03;
    text_slant = Cairo.FONT_SLANT_NORMAL;
    text_weight = Cairo.FONT_WEIGHT_NORMAL;
    text_color = black;
  }

let default_label_style =
  (** The default style for the x and y axis labels and the title
      text. *)
  {
    text_font = "Palatino-Roman";
    text_size = 0.04;
    text_slant = Cairo.FONT_SLANT_NORMAL;
    text_weight = Cairo.FONT_WEIGHT_NORMAL;
    text_color = black;
  }


let text_padding = 0.01
  (** Padding around text *)


class virtual plot =
  (** [plot] a plot has a method for drawing. *)
object
  method virtual draw : context -> unit
    (** [draw ctx] displays the plot to the given drawing
	context. *)
end

(** {2 Numeric by numeric plot} ****************************************)

class num_by_num_plot
  ?(label_style=default_label_style)
  ?(tick_style=default_tick_style)
  ~title ~xlabel ~ylabel ?scale datasets =
  (** [num_by_num_plot ?label_style ?tick_style ~title ~xlabel ~ylabel
      ?scale datasets] a plot that has a numeric x and y axis. *)
object (self)
  inherit plot

  val datasets = datasets
    (** The list of datasets. *)

  method private scale =
    (** [scale] computes the scale of the x and y axes. *)
    match scale with
      | None -> failwith "Automatic dimensions is currently unimplemented"
      | Some rect -> rect


  method private xticks =
    (** [xticks] computes the location of the x-axis tick marks. *)
    let s = self#scale in
      Numeric_axis.tick_locations s.x_min s.x_max


  method private yticks =
    (** [yticks] computes the location of the y-axis tick marks. *)
    let s = self#scale in
      Numeric_axis.tick_locations s.y_min s.y_max


  method private dest_rect ctx =
    (** [dest_rect ctx] get the dimensions of the destination
	rectangle. *)
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:label_style txt) in
    let src = self#scale in
    let y_min', x_max' =
      Numeric_axis.resize_for_x_axis
	ctx ~label_style ~tick_style ~pad:text_padding
	~y_min:1.
	~x_min:src.x_min ~x_max:src.x_max ~x_min':0. ~x_max':1.
	xlabel self#xticks
    in
    let x_min' =
      Numeric_axis.resize_for_y_axis ctx ~label_style ~tick_style
	~pad:text_padding ~x_min:0. ylabel self#yticks
    in
      rectangle x_min' x_max' y_min' (title_height +. text_padding)


  method private draw_x_axis ctx ~src ~dst =
    (** [draw_x_axis ctx ~src ~dst] draws the x-axis. *)
    Numeric_axis.draw_x_axis ctx
      ~tick_style ~label_style ~pad:text_padding
      ~y:1.
      ~x_min:src.x_min ~x_max:src.x_max
      ~x_min':dst.x_min ~x_max':dst.x_max
      xlabel self#xticks


  method private draw_y_axis ctx ~src ~dst =
    (** [draw_y_axis ctx ~src ~dst] draws the y-axis. *)
      Numeric_axis.draw_y_axis ctx
	~tick_style ~label_style ~pad:text_padding
	~x:0.
	~y_min:src.y_min ~y_max:src.y_max
	~y_min':dst.y_min ~y_max':dst.y_max
	ylabel self#yticks


  method draw ctx =
    (** [draw ctx] draws the numeric by numeric plot to the given
	context. *)
    let src = self#scale in
    let dst = self#dest_rect ctx in
    let tr = transform ~src ~dst in
    let rank = ref 0 in
      begin match title with
	| None -> ()
	| Some t -> draw_text_centered_below ~style:label_style ctx 0.5 0. t
      end;
      self#draw_x_axis ctx ~src ~dst;
      self#draw_y_axis ctx ~src ~dst;
      List.iter (fun ds -> ds#draw ctx tr dst !rank; incr rank) datasets

end


(** {2 Numeric by nominal plot} ****************************************)

class num_by_nom_plot
  ?(label_style=default_label_style)
  ?(tick_style=default_tick_style)
  ~title ~ylabel ?y_min ?y_max datasets =
  (** [num_by_nom_plot ?label_style ?tick_style ~title ~ylabel ?y_min
      ?y_max datasets] a plot that has a nominal x axis and a numeric
      y axis. *)
object (self)
  inherit plot

  val datasets = datasets

  method private scale =
    (** [scale] computes the scale of the y axis. *)
    match y_min, y_max with
      | Some min, Some max -> min, max
      | _ ->
	  List.fold_left (fun (min, max) ds ->
			    let ds_min, ds_max = ds#y_min_and_max in
			    let min' = if ds_min < min then ds_min else min
			    and max' = if ds_max > max then ds_max else max
			    in min', max')
	    (infinity, neg_infinity) datasets


  method private yticks =
    (** [yticks] computes the location of the y-axis tick marks. *)
    let y_min, y_max = self#scale in
      Numeric_axis.tick_locations y_min y_max


  method private x_axis_dimensions ctx =
    (** [x_axis_dimensions ctx] computes the x_min, x_max and width
	for each dataset to display its name on the x-axis. *)
    let x_max = 1. in
    let x_min =
      Numeric_axis.resize_for_y_axis ctx ~label_style ~tick_style
	~pad:text_padding ~x_min:0. ylabel self#yticks
    in
    let n = List.length datasets in
    let width = if n > 0 then (x_max -. x_min) /. (float n) else 0. in
      x_min, x_max, width


  method private dest_y_scale ctx ~y_min ~y_max ~width =
    (** [dest_y_scale ctx ~y_min ~y_max ~width] get the scale on the
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
      ((1. -. Numeric_axis.axis_padding -. data_label_height),
       (title_height +. text_padding))



  method private draw_y_axis ctx ~y_min ~y_max ~y_min' ~y_max' =
    (** [draw_y_axis ctx ~y_min ~y_max ~y_min' ~y_max'] draws the
	y-axis. *)
    Numeric_axis.draw_y_axis ctx
      ~tick_style ~label_style ~pad:text_padding
      ~x:0. ~y_min ~y_max ~y_min' ~y_max' ylabel self#yticks


  method private draw_x_axis ctx ~y ~x_min ~x_max ~width =
    (** [draw_x_axis ctx ~y ~x_min ~x_max ~width] draws the x-axis. *)
    set_text_style ctx tick_style;
    ignore (List.fold_left
	      (fun x ds ->
		 ds#draw_x_label ctx ~x ~y ~width;
		 x +. width)
	      (x_min +. (width /. 2.)) datasets)


  method draw ctx =
    (** [draw ctx] draws the plot. *)
    let y_min, y_max = self#scale in
    let x_min, x_max, width = self#x_axis_dimensions ctx in
    let y_min', y_max' = self#dest_y_scale ctx ~y_min ~y_max ~width in
      begin match title with
	| None -> ()
	| Some t -> draw_text_centered_below ~style:label_style ctx 0.5 0. t
      end;
      self#draw_y_axis ctx ~y_min ~y_max ~y_min' ~y_max';
      self#draw_x_axis ctx
	~y:(y_min' +. Numeric_axis.axis_padding) ~x_min ~x_max ~width
end


(** {1 Datasets} ****************************************)

class virtual num_by_num_dataset name =
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
    context -> (point -> point) -> rectangle -> int -> unit
    (** [draw ctx transform dst rank] draws the data to the plot *)

  method virtual draw_legend_entry : context -> x:float -> y:float -> float
    (** [draw_legend_entry ctx ~x ~y] draws the legend entry to the
	given location ([x] is the left-edge and [y] is top edge of the
	destination) and the result is the y-coordinate of the bottom edge
	of the entry that was just drawn. *)
end

class virtual num_by_nom_dataset name =
  (** [num_by_nom_dataset name] is a dataset that is plottable on a
      nominal x axis and a numeric y axis. *)
object

  val name = (name : string)
    (** The name of the dataset is what appears on the x-axis. *)

  method virtual y_min_and_max : float * float
    (** [y_min_and_max] gets the min and maximum value from the
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


  method virtual draw :
    context -> (float -> float) -> y_min:float -> y_max:float
    -> width:float -> int -> unit
    (** [draw ctx scale ~y_min ~y_max ~width rank] draws the dataset
	to the plot. *)
end
