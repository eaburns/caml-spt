(** Numeric by numeric plots.

    @author eaburns
    @since 2010-04-25
*)

open Geometry
open Drawing
open Verbosity

let axis_padding = Length.Pt 5.
  (** The padding between the axis and the data. *)


let data_dimensions ~x_min ~x_max ~y_min ~y_max datasets =
  (** [data_dimensions ~x_min ~x_max ~y_min ~y_max datasets] computes
      the range of the x and y axes for the given datasets. *)
  let r = match datasets with
    | d :: [] -> d#dimensions
    | d :: ds ->
	List.fold_left (fun r d -> rectangle_extremes r d#dimensions)
	  d#dimensions ds
    | [] ->
	rectangle ~x_min:infinity ~x_max:neg_infinity
	  ~y_min:infinity ~y_max:neg_infinity
  in
  let x_pad = range_padding ~max:r.x_max ~min:r.x_min 0.01 in
  let y_pad = range_padding ~max:r.y_max ~min:r.y_min 0.01 in
  let x_min' = match x_min with None -> (r.x_min -. x_pad) | Some m -> m
  and x_max' = match x_max with None -> (r.x_max +. x_pad) | Some m -> m
  and y_min' = match y_min with None -> (r.y_min -. y_pad) | Some m -> m
  and y_max' = match y_max with None -> (r.y_max +. y_pad) | Some m -> m in
  let r = rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max' in
    vprintf verb_normal "data dimensions: x=[%f, %f], y=[%f, %f]\n"
      r.x_min r.x_max r.y_min r.y_max;
    r


(** {1 Numeric by numeric plot} ****************************************)

class plot
  ?(label_text_style=Spt.default_label_style)
  ?(legend_text_style=Spt.default_legend_style)
  ?(tick_text_style=Spt.default_tick_style)
  ?title ?xlabel ?ylabel
  ?(legend_loc=Legend.Lower_right)
  ?x_min ?x_max ?y_min ?y_max
  datasets =
  (** [plot ?label_style ?legend_style ?tick_style ?title ?xlabel
      ?ylabel ?x_min ?x_max ?y_min ?y_max datasets] a plot that has a
      numeric x and y axis. *)
  let _ = vprintf verb_normal "creating a numeric by numeric plot\n" in
object (self)
  inherit Spt.plot title

  val src = data_dimensions ~x_min ~x_max ~y_min ~y_max datasets
    (** The dimensions of the axes in the data coordinate system. *)


  method suggest_aspect =
    let avg_slopes = List.map (fun ds -> ds#avg_slope) datasets in
    let avg_slope = ((List.fold_left (+.) 0. avg_slopes) /.
		       (float (List.length datasets))) in
      avg_slope

  method use_suggested_aspect =
    let skew len s =
      match len with
	| Length.In flt -> Length.In (flt *. s)
	| Length.Cm flt -> Length.Cm (flt *. s)
	| Length.Px i -> Length.Px (truncate ((float i) *. s))
	| Length.Pt flt -> Length.Pt (flt *. s) in
    let ratio = self#suggest_aspect in
      if ratio <= 1. (* plot is tall, use plot height to determine size*)
      then self#set_size ~w:(skew self#width ratio) ~h:self#height
      else self#set_size ~w:self#width ~h:(skew self#height ratio)


  method private dst_rectangle ctx ~xaxis ~yaxis =
    (** [dst_rectangle ctx ~xaxis ~yaxis] get the dimensions of
	the destination rectangle. *)
    let axis_padding = ctx.units axis_padding in
    let xsize, ysize = self#size ctx in
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:label_text_style txt) in
    let y_min', x_max' =
      Numeric_axis.resize_for_x_axis
	ctx ~pad:(ctx.units Spt.text_padding) ~y_min:(ysize -. axis_padding)
	~dst:(range 0. xsize) xaxis in
    let x_min' =
      Numeric_axis.resize_for_y_axis ctx
	~pad:(ctx.units Spt.text_padding) ~x_min:axis_padding yaxis in
    let dst =
      rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min'
	~y_max:(title_height +. (ctx.units Spt.text_padding)) in
    let residual =
      (* Maximum distance over the edge of the [dst] rectangle that
	 any dataset may need to draw. *)
      List.fold_left
	(fun r ds -> rectangle_max r (ds#residual ctx ~src ~dst))
	zero_rectangle datasets in
    let r = (rectangle ~x_min:(dst.x_min +. residual.x_min)
	       ~x_max:(dst.x_max -. residual.x_max)
	       ~y_min:(dst.y_min -. residual.y_min)
	       ~y_max:(dst.y_max +. residual.y_max))
    in
      vprintf verb_optional "plot dimensions: x=[%f, %f], y=[%f, %f]\n"
	r.x_min r.x_max r.y_min r.y_max;
      r


  method private xaxis =
    (** [xaxis] creates the x-axis for the plot. *)
    let src = xrange src in
    let nticks = Numeric_axis.recommended_ticks width in
    let ticks = Numeric_axis.tick_locations ~suggested_number:nticks src in
      verb_eval verb_debug
	(fun () ->
	   vprintf verb_debug "x-ticks:\n";
	   List.iter (fun (vl, name) -> match name with
			| None -> vprintf verb_debug "\tminor: %f\n" vl
			| Some _ -> vprintf verb_debug "\tmajor: %f\n" vl)
	     ticks);
      Numeric_axis.create ~label_text_style ~tick_text_style ~src ticks xlabel


  method private yaxis =
    (** [yaxis] creates the y-axis for the plot. *)
    let src = yrange src in
    let nticks = Numeric_axis.recommended_ticks height in
    let ticks = Numeric_axis.tick_locations ~suggested_number:nticks src in
      verb_eval verb_debug
	(fun () ->
	   vprintf verb_debug "y-ticks:\n";
	   List.iter (fun (vl, name) -> match name with
			| None -> vprintf verb_debug "\tminor: %f\n" vl
			| Some _ -> vprintf verb_debug "\tmajor: %f\n" vl)
	     ticks);
      Numeric_axis.create ~label_text_style ~tick_text_style ~src ticks ylabel


  method private draw_x_axis ctx ~dst xaxis =
    (** [draw_x_axis ctx ~dst xaxis] draws the
	x-axis. *)
    let xsize, ysize = self#size ctx in
      Numeric_axis.draw_x_axis ctx ~pad:(ctx.units Spt.text_padding)
	~width:xsize ~height:ysize ~dst:(xrange dst) xaxis


  method private draw_y_axis ctx ~dst yaxis =
    (** [draw_y_axis ctx ~dst] draws the y-axis. *)
    let xsize, ysize = self#size ctx in
      Numeric_axis.draw_y_axis ctx
	~pad:(ctx.units Spt.text_padding) ~width:xsize ~height:ysize
	~dst:(yrange dst) yaxis


  method draw ctx =
    vprintf verb_optional "drawing numeric by numeric plot\n";
    self#fill_background ctx;
    let axis_padding = ctx.units axis_padding in
    let xaxis = self#xaxis and yaxis = self#yaxis in
    let dst = self#dst_rectangle ctx ~xaxis ~yaxis in
    let legend_txt_loc, legend_x, legend_y =
      let legend_dst = { dst with
			   y_min = dst.y_min +. axis_padding;
			   x_min = (dst.x_min -. axis_padding
				    +. (ctx.units Spt.text_padding)); }
      in Legend.locate ctx legend_text_style legend_dst datasets legend_loc
    in
      begin match title with
	| None -> ()
	| Some t ->
	    let x = (fst (self#size ctx)) /. 2. and y = 0. in
	      draw_text_centered_below ~style:label_text_style ctx x y t
      end;
      self#draw_x_axis ctx ~dst xaxis;
      self#draw_y_axis ctx ~dst yaxis;
      List.iter (fun ds -> ds#draw ctx ~src ~dst) datasets;
      save_transforms ctx;
      translate ctx legend_x legend_y;
      Legend.draw ctx legend_txt_loc legend_text_style datasets;
      restore_transforms ctx

end

include Num_by_num_dataset
include Label_dataset
include Errbar_dataset
include Scatter_dataset
include Line_dataset
include Bubble_dataset
include Line_errbar_dataset
include Function_dataset
include Histogram_dataset
include Cdf_dataset
include Num_heatmap_dataset
