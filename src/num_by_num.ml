(** Numeric by numeric plots.

    TODO: The scatter dataset should make room for the glyph radius
    when it says its dimensions.

    @author eaburns
    @since 2010-04-25
*)

open Geometry
open Drawing

(** {1 Numeric by numeric plot} ****************************************)

class plot
  ?(label_style=Ml_plot.default_label_style)
  ?(tick_style=Ml_plot.default_tick_style)
  ?title ?xlabel ?ylabel
  ?x_min ?x_max ?y_min ?y_max datasets =
  (** [plot ?label_style ?tick_style ?title ?xlabel ?ylabel ?x_min
      ?x_max ?y_min ?y_max datasets] a plot that has a numeric x and y
      axis. *)
object (self)
  inherit Ml_plot.plot

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


  method private dest_rectangle ctx =
    (** [dest_rectangle ctx] get the dimensions of the destination
	rectangle. *)
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:label_style txt) in
    let src = self#ranges in
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
    let residual, _ =
      (* Maximum distance over the edge of the [dst] rectangle that
	 any dataset may need to draw. *)
      List.fold_left
	(fun (r, rank) ds ->
	   rectangle_max r (ds#residual ctx ~src ~dst rank), rank + 1)
	(zero_rectangle, 0) datasets
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


  method draw ctx =
    (** [draw ctx] draws the numeric by numeric plot to the given
	context. *)
    let src = self#ranges in
    let dst = self#dest_rectangle ctx in
      begin match title with
	| None -> ()
	| Some t -> draw_text_centered_below ~style:label_style ctx 0.5 0. t
      end;
      self#draw_x_axis ctx ~src ~dst;
      self#draw_y_axis ctx ~src ~dst;
      let rank = ref 0 in
	List.iter (fun ds -> ds#draw ctx ~src ~dst !rank; incr rank) datasets

end

include Errbar_dataset
include Scatter_dataset
include Line_dataset
include Bubble_dataset
