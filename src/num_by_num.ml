(** Numeric by numeric plots.

    @author eaburns
    @since 2010-04-25
*)

open Geometry
open Drawing

type legend_text_location =
  | Text_before
  | Text_after


type legend_location =
  | Legend_at of legend_text_location * float * float
  | Legend_upper_left
  | Legend_lower_left
  | Legend_upper_right
  | Legend_lower_right


let legend_icon_width = 0.08
  (** The width of a legend icon. *)



(** {1 Numeric by numeric plot} ****************************************)

class plot
  ?(label_style=Ml_plot.default_label_style)
  ?(legend_style=Ml_plot.default_legend_style)
  ?(tick_style=Ml_plot.default_tick_style)
  ?title ?xlabel ?ylabel
  ?(legend_loc=Legend_lower_right)
  ?x_min ?x_max ?y_min ?y_max
  datasets =
  (** [plot ?label_style ?legend_style ?tick_style ?title ?xlabel
      ?ylabel ?x_min ?x_max ?y_min ?y_max datasets] a plot that has a
      numeric x and y axis. *)
object (self)
  inherit Ml_plot.plot title

  val datasets = datasets
    (** The list of datasets. *)


  method private ranges =
    (** [ranges] computes the range of the x and y axes. *)
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


  method private xticks =
    (** [xticks] computes the location of the x-axis tick marks. *)
    Numeric_axis.tick_locations (xrange self#ranges)


  method private yticks =
    (** [yticks] computes the location of the y-axis tick marks. *)
    Numeric_axis.tick_locations (yrange self#ranges)


  method private dest_rectangle src ctx =
    (** [dest_rectangle src ctx] get the dimensions of the destination
	rectangle. *)
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:label_style txt) in
    let y_min', x_max' =
      Numeric_axis.resize_for_x_axis
	ctx ~label_style ~tick_style ~pad:Ml_plot.text_padding
	~y_min:1. ~src:(xrange src) ~dst:(range 0. 1.) xlabel self#xticks in
    let x_min' =
      Numeric_axis.resize_for_y_axis ctx ~label_style ~tick_style
	~pad:Ml_plot.text_padding ~x_min:0. ylabel self#yticks in
    let dst =
      rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min'
	~y_max:(title_height +. Ml_plot.text_padding) in
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


  method private draw_x_axis ctx ~src ~dst =
    (** [draw_x_axis ctx ~src ~dst] draws the x-axis. *)
    Numeric_axis.draw_x_axis ctx
      ~tick_style ~label_style ~pad:Ml_plot.text_padding
      ~y:1. ~src:(xrange src) ~dst:(xrange dst) xlabel self#xticks


  method private draw_y_axis ctx ~src ~dst =
    (** [draw_y_axis ctx ~src ~dst] draws the y-axis. *)
    Numeric_axis.draw_y_axis ctx
      ~tick_style ~label_style ~pad:Ml_plot.text_padding
      ~x:0. ~src:(yrange src) ~dst:(yrange dst) ylabel self#yticks


  method private legend_size ?(style=legend_style) ctx =
    (** [legend_size ctx] computes the size of the legend. *)
    let pad = Ml_plot.text_padding in
      List.fold_left
	(fun ((w, h) as dims) ds -> match ds#name with
	   | None -> dims
	   | Some txt ->
	       let txt_w, txt_h = text_dimensions ctx ~style txt in
	       let width = txt_w +. legend_icon_width +. pad in
	       let height = txt_h +. pad in
	       let w' = if width > w then width else w
	       and h' = if height > h then height else h
	       in w', h')
	(0., 0.) datasets


  method private locate_legend ctx dst = match legend_loc with
      (** [locate_legend ctx dst] gets the location for drawing the
	  plot legend. *)
    | Legend_at (txt_loc, x, y) ->
	txt_loc, x, y
    | Legend_upper_left ->
	Text_after, dst.x_min, dst.y_max
    | Legend_lower_left ->
	let _, h = self#legend_size ctx in
	  Text_after, dst.x_min, dst.y_min -. h
    | Legend_upper_right ->
	let w, _ = self#legend_size ctx in
	  Text_before, dst.x_max -. w, dst.y_max
    | Legend_lower_right ->
	let w, h = self#legend_size ctx in
	  Text_before, dst.x_max -. w, dst.y_min -. h



  method draw ctx =
    (** [draw ctx] draws the numeric by numeric plot to the given
	context. *)
    let src = self#ranges in
    let dst = self#dest_rectangle src ctx in
    let legend_txt_loc, legend_x, legend_y = self#locate_legend ctx dst in
      begin match title with
	| None -> ()
	| Some t -> draw_text_centered_below ~style:legend_style ctx 0.5 0. t
      end;
      self#draw_x_axis ctx ~src ~dst;
      self#draw_y_axis ctx ~src ~dst;
      List.iter (fun ds -> ds#draw ctx ~src ~dst) datasets;
      save_transforms ctx;
      translate ctx legend_x legend_y;
      self#draw_legend ctx ~text_loc:legend_txt_loc;
      restore_transforms ctx


  method draw_legend
    ?(text_loc=Text_before) ?(style=legend_style) (ctx:context) =
    (** [draw_legend ?text_loc ?style ctx] draws the legend into the
	upper right corner of the unit square. *)
    let pad = Ml_plot.text_padding in
      ignore (List.fold_left
		(fun y ds -> match ds#name with
		   | None -> y
		   | Some txt ->
		       let w, h = text_dimensions ctx ~style txt in
		       let x = w /. 2. and y' = y +. (h /. 2.) in
		       let x', rect = match text_loc with
			 | Text_before ->
			     x, (rectangle
				   ~x_min:(w +. pad)
				   ~x_max:(w +. pad +. legend_icon_width)
				   ~y_min:y
				   ~y_max:(y +. h))
			 | Text_after ->
			     x +. pad +. legend_icon_width,
			     (rectangle
				~x_min:0. ~x_max:legend_icon_width
				~y_min:y ~y_max:(y +. h))
		       in
			 draw_text ctx ~style x' y' txt;
			 ds#draw_legend_entry ctx rect;
			 y +. h +. pad;)
		0. datasets)


end

include Num_by_num_dataset
include Label_dataset
include Errbar_dataset
include Scatter_dataset
include Line_dataset
include Bubble_dataset
include Line_errbar_dataset
