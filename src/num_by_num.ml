(** Numeric by numeric plots.

    @author eaburns
    @since 2010-04-25
*)

open Geometry
open Drawing

let axis_padding = Length.Pt 5.
  (** The padding between the axis and the data. *)

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
object (self)
  inherit Spt.plot title

  val datasets = datasets
    (** The list of datasets. *)


  method private src_rectangle =
    (** [src_rectangle] computes the range of the x and y axes. *)
    let r = match datasets with
      | d :: [] -> d#dimensions
      | d :: ds ->
	  List.fold_left (fun r d -> rectangle_extremes r d#dimensions)
	    d#dimensions ds
      | [] ->
	  rectangle ~x_min:infinity ~x_max:neg_infinity
	    ~y_min:infinity ~y_max:neg_infinity
    in
    let x_min' = match x_min with None -> r.x_min | Some m -> m
    and x_max' = match x_max with None -> r.x_max | Some m -> m
    and y_min' = match y_min with None -> r.y_min | Some m -> m
    and y_max' = match y_max with None -> r.y_max | Some m -> m
    in rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max'


  method private dst_rectangle ctx ~xaxis ~yaxis src =
    (** [dst_rectangle ctx ~xaxis ~yaxis src] get the dimensions of
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
	zero_rectangle datasets
    in
      rectangle
	~x_min:(dst.x_min +. residual.x_min)
	~x_max:(dst.x_max -. residual.x_max)
	~y_min:(dst.y_min -. residual.y_min)
	~y_max:(dst.y_max +. residual.y_max)


  method private xaxis src =
    (** [xaxis src] creates the x-axis for the plot. *)
    let nticks = Numeric_axis.recommended_ticks width in
    let ticks =
      Numeric_axis.tick_locations ~suggested_number:nticks
	(xrange self#src_rectangle)
    in
      Numeric_axis.create ~label_text_style ~tick_text_style
	~src:(xrange src) ticks xlabel


  method private yaxis src =
    (** [yaxis src] creates the y-axis for the plot. *)
    let nticks = Numeric_axis.recommended_ticks height in
    let ticks =
      Numeric_axis.tick_locations ~suggested_number:nticks
	(yrange self#src_rectangle)
    in
      Numeric_axis.create ~label_text_style ~tick_text_style
	~src:(yrange src) ticks ylabel


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
    let src = self#src_rectangle in
    let axis_padding = ctx.units axis_padding in
    let xaxis = self#xaxis src and yaxis = self#yaxis src in
    let dst = self#dst_rectangle ctx ~xaxis ~yaxis src in
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
