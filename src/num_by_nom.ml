(** Numeric by nominal plots.

    @author eaburns
    @since 2010-04-26
*)

open Geometry
open Drawing
open Verbosity

let y_axis_padding = Length.Pt 10.
  (** The amount of room to separate the y-axis from the data. *)

let x_axis_padding = Length.Pt 5.
  (** The amount of room to separate the x-axis from the data. *)

let between_padding = Length.Pt 10.
  (** Padding between each dataset. *)

let data_dimensions ~y_min ~y_max datasets =
  (** [data_dimensions ~y_min ~y_max datasets] computes the dimension
      of the y-axis. *)
  let min, max =
    List.fold_left (fun (min, max) ds ->
		      let r = ds#dimensions in
		      let ds_min = r.min and ds_max = r.max in
		      let min' = if ds_min < min then ds_min else min
		      and max' = if ds_max > max then ds_max else max
		      in min', max')
      (infinity, neg_infinity) datasets
  in
  let pad = range_padding ~max ~min 0.01 in
  let min = match y_min with None -> (min -. pad) | Some m -> m in
  let max = match y_max with None -> (max +. pad) | Some m -> m in
  let r = range ~min ~max in
    vprintf verb_optional "data dimensions: y=[%f, %f]\n" r.min r.max;
    r

(** {1 Numeric by nomeric plot} ****************************************)

class plot
  ?(label_text_style=Spt.default_label_style)
  ?(legend_text_style=Spt.default_legend_style)
  ?(tick_text_style=Spt.default_tick_style)
  ?title ?ylabel ?y_min ?y_max datasets =
  (** [plot ?label_style ?legend_style ?tick_style ?title ?ylabel
      ?y_min ?y_max datasets] a plot that has a nominal x axis and a
      numeric y axis. *)
  let _ = vprintf verb_optional "creating a numeric by nominal plot\n" in
object (self)
  inherit Spt.plot title

  val src = data_dimensions ~y_min ~y_max datasets
    (** The dimensions of the y-axis in data coordinates. *)

  method private yaxis =
    (** [yaxis] creates a y-axis. *)
    let nticks = Numeric_axis.recommended_ticks height in
    let ticks = Numeric_axis.tick_locations ~suggested_number:nticks src in
      verb_eval verb_debug
	(fun () ->
	   vprintf verb_debug "y-ticks:\n";
	   List.iter (fun (vl, name) -> match name with
			| None -> vprintf verb_debug "\tminor: %f\n" vl
			| Some _ -> vprintf verb_debug "\tmajor: %f\n" vl)
	     ticks);
      Numeric_axis.create ~label_text_style ~tick_text_style
	~src ticks ylabel


  method private x_axis_dimensions ctx yaxis =
    (** [x_axis_dimensions ctx] computes the x_min, x_max and width
	for each dataset to display its name on the x-axis. *)
    let x_max, _ = self#size ctx in
    let x_min =
      Numeric_axis.resize_for_y_axis ctx
	~pad:(ctx.units Spt.text_padding) ~x_min:(ctx.units y_axis_padding)
	yaxis
    in
    let n = float (List.length datasets) in
    let text_width =
      let total_padding = (n -. 1.) *. (ctx.units between_padding) in
	if n > 1.
	then ((x_max -. x_min) -. total_padding) /. n
	else (x_max -. x_min)
    in
      range x_min x_max, text_width


  method private dst_y_range ctx ~y_min ~y_max ~text_width =
    (** [dst_y_range ctx ~y_min ~y_max ~text_width] get the range on
	the y-axis.  [text_width] is the amount of width afforded to
	each dataset on the x-axis. *)
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:tick_text_style txt) in
    let data_label_height =
      List.fold_left (fun m ds ->
			let h =
			  ds#x_label_height ctx legend_text_style text_width
			in if h > m then h else m)
	0. datasets
    in
      range
	((snd (self#size ctx)) -. data_label_height
	 -. (ctx.units x_axis_padding))
	(title_height +. (2. *. (ctx.units Spt.text_padding)))



  method private draw_y_axis ctx ~dst yaxis =
    (** [draw_y_axis ctx ~dst yaxis] draws the y-axis. *)
    let xsize, ysize = self#size ctx in
      Numeric_axis.draw_y_axis ctx ~pad:(ctx.units Spt.text_padding)
	~width:xsize ~height:ysize ~dst yaxis;


  method private draw_x_axis ctx ~y ~xrange ~text_width =
    (** [draw_x_axis ctx ~y ~xrange ~text_width] draws the x-axis. *)
    let between_padding = ctx.units between_padding in
      ignore
	(List.fold_left
	   (fun x ds ->
	      ds#draw_x_label ctx ~x ~y legend_text_style ~width:text_width;
	      x +. text_width +. between_padding)
	   xrange.min datasets)


  method draw ctx =
    vprintf verb_debug "drawing numeric by nominal plot\n";
    self#fill_background ctx;
    let between_padding = ctx.units between_padding in
    let yaxis = self#yaxis in
    let xrange, width = self#x_axis_dimensions ctx yaxis in
    let dst = self#dst_y_range ctx ~y_min ~y_max ~text_width:width in
      vprintf verb_debug
	"plot dimensions: x=[%f, %f], y=[%f, %f]\ntext width=%f\n"
	xrange.min xrange.max dst.min dst.max width;
      begin match title with
	| None -> ()
	| Some t ->
	    let x = (fst (self#size ctx)) /. 2. and y = 0. in
	      draw_text_centered_below ~style:label_text_style ctx x y t
      end;
      ignore (List.fold_left (fun x ds ->
				ds#draw ctx ~src ~dst ~width ~x;
				x +. width +. between_padding)
		xrange.min datasets);
      self#draw_y_axis ctx ~dst yaxis;
      self#draw_x_axis ctx ~y:(dst.min +. (ctx.units x_axis_padding))
	~xrange ~text_width:width
end

include Num_by_nom_dataset
include Boxplot_dataset
include Barchart_dataset
