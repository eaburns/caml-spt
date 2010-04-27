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


  method private scale =
    (** [scale] computes the scale of the x and y axes. *)
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
    let s = self#scale in
      Numeric_axis.tick_locations s.x_min s.x_max


  method private yticks =
    (** [yticks] computes the location of the y-axis tick marks. *)
    let s = self#scale in
      Numeric_axis.tick_locations s.y_min s.y_max


  method private dest_rectangle ctx =
    (** [dest_rectangle ctx] get the dimensions of the destination
	rectangle. *)
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:label_style txt) in
    let src = self#scale in
    let y_min', x_max' =
      Numeric_axis.resize_for_x_axis
	ctx ~label_style ~tick_style ~pad:Ml_plot.text_padding
	~y_min:1.
	~x_min:src.x_min ~x_max:src.x_max ~x_min':0. ~x_max':1.
	xlabel self#xticks in
    let x_min' =
      Numeric_axis.resize_for_y_axis ctx ~label_style ~tick_style
	~pad:Ml_plot.text_padding ~x_min:0. ylabel self#yticks in
    let dst =
      rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min'
	~y_max:(title_height +. Ml_plot.text_padding) in
    let residue, _ =
      (* Maximum distance over the edge of the [dst] rectangle that
	 any dataset may need to draw. *)
      List.fold_left
	(fun (r, rank) ds ->
	   rectangle_max r (ds#residue ctx ~src ~dst rank), rank + 1)
	(zero_rectangle, 0) datasets
    in
      rectangle
	~x_min:(dst.x_min +. residue.x_min)
	~x_max:(dst.x_max -. residue.x_max)
	~y_min:(dst.y_min -. residue.y_min)
	~y_max:(dst.y_max +. residue.y_max)


  method private draw_x_axis ctx ~src ~dst =
    (** [draw_x_axis ctx ~src ~dst] draws the x-axis. *)
    Numeric_axis.draw_x_axis ctx
      ~tick_style ~label_style ~pad:Ml_plot.text_padding
      ~y:1.
      ~x_min:src.x_min ~x_max:src.x_max
      ~x_min':dst.x_min ~x_max':dst.x_max
      xlabel self#xticks


  method private draw_y_axis ctx ~src ~dst =
    (** [draw_y_axis ctx ~src ~dst] draws the y-axis. *)
    Numeric_axis.draw_y_axis ctx
      ~tick_style ~label_style ~pad:Ml_plot.text_padding
      ~x:0.
      ~y_min:src.y_min ~y_max:src.y_max
      ~y_min':dst.y_min ~y_max':dst.y_max
      ylabel self#yticks


  method draw ctx =
    (** [draw ctx] draws the numeric by numeric plot to the given
	context. *)
    let src = self#scale in
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

(** {1 Datasets} ****************************************)


class virtual dataset ?name () =
  (** [dataset name] is a dataset that is plottable on a numeric x and
      y axis. *)
object
  val name = (name : string option)
    (** The name of the dataset.  If there is no name then the dataset
	doesn't appear in the legend. *)

  method virtual dimensions : rectangle
    (** [dimensions] is the dimensions of this dataset in
	data-coordinates. *)

  method virtual residue :
    context -> src:rectangle -> dst:rectangle -> int -> rectangle
    (** [residue ctx ~src ~dst rank] get a rectangle containing the
	maximum amount the dataset will draw off of the destination
	rectangle in each direction. *)

  method virtual draw :
    context -> src:rectangle -> dst:rectangle -> int -> unit
    (** [draw ctx ~src ~dst rank] draws the data to the plot. *)

  method virtual draw_legend_entry : context -> x:float -> y:float -> float
    (** [draw_legend_entry ctx ~x ~y] draws the legend entry to the
	given location ([x] is the left-edge and [y] is top edge of
	the destination) and the result is the y-coordinate of the
	bottom edge of the entry that was just drawn. *)
end


(** {2 Points datasets} ****************************************)


class virtual points_dataset ?name points =
  (** A dataset composed of a set of points. *)
object
  inherit dataset ?name ()

  val points = (points : point list)
    (** The list of points. *)

  method dimensions = points_rectangle points
    (** [dimensions] gets the rectangle around the points. *)
end


(** {3 Scatter dataset} ****************************************)


let glyphs =
  (** The default glyphs for scatter plots. *)
  [| Circle_glyph;
     Ring_glyph;
     Plus_glyph;
     Triangle_glyph;
     Box_glyph;
     Square_glyph;
     Cross_glyph;
  |]


(*
let glyphs =
  (** The default glyphs for scatter plots. *)
  [| Char_glyph '1';
     Char_glyph '2';
     Char_glyph '3';
     Char_glyph '4';
     Char_glyph '5';
     Char_glyph '6';
     Char_glyph '7';
  |]
*)

class scatter_dataset ?glyph ?(color=black) ?(radius=0.012) ?name points =
  (** A scatter plot dataset. *)
object (self)
  inherit points_dataset ?name points

  method glyph rank = match glyph with
      (** [glyph rank] the glyph to use for this dataset. *)
    | None -> glyphs.(rank)
    | Some g -> g


  method residue ctx ~src ~dst _ =
    (** [residue ctx ~src ~dst rank] if we were to plot this right now
	with the given [dst] rectangle, how far out-of-bounds will we
	go in each direction. *)
    let tr = transform ~src ~dst in
      List.fold_left
	(fun r pt ->
	   if rectangle_contains src pt
	   then rectangle_max r (point_residue dst (tr pt) radius)
	   else r)
	zero_rectangle points


  method draw ctx ~src ~dst rank =
    let tr = transform ~src ~dst in
    let pts = List.map tr (List.filter (rectangle_contains src) points) in
      draw_points ctx ~color radius (self#glyph rank) pts

  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end

(** {3 Line dataset} ****************************************)


let dashes =
  (** The dash patterns for lines. *)
  [|
    [| |];
    [| 0.01; 0.01; |];
    [| 0.02; 0.02; |];
    [| 0.04; 0.01; |];
    [| 0.03; 0.02; 0.01; 0.02; |];
    [| 0.03; 0.01; 0.01; 0.01; 0.01; 0.01; |];
    [| 0.04; 0.005; 0.005; 0.005; 0.005; 0.005; 0.005; 0.005; |];
  |]


class line_dataset ?dash_pattern ?(width=0.002) ?(color=black) ?name points =
  (** A line plot dataset. *)
object (self)
  inherit points_dataset ?name points

  method style rank =
    (** [style rank] get the style of the line *)
    {
      line_color = color;
      line_dashes = begin match dash_pattern with
	| None -> dashes.(rank mod (Array.length dashes))
	| Some d -> d
      end;
      line_width = width;
    }

  method residue _ ~src:_ ~dst _ = zero_rectangle

  method draw ctx ~src ~dst rank =
    let tr = transform ~src ~dst in
    let pts = List.map tr points in
      draw_line ctx ~box:dst ~style:(self#style rank) pts

  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end


(** {2 Line points dataset} ****************************************)


class line_points_dataset
  ?dash_pattern ?width ?glyph ?radius ?color
  ?name points =
  (** A line with points plot dataset. *)
object
  inherit dataset ?name ()

  val line = new line_dataset ?dash_pattern ?width ?color points
  val scatter = new scatter_dataset ?glyph ?radius ?color points

  method dimensions = rectangle_extremes scatter#dimensions line#dimensions

  method residue ctx ~src ~dst rank =
    rectangle_extremes
      (line#residue ctx ~src ~dst rank)
      (scatter#residue ctx ~src ~dst rank)

  method draw ctx ~src ~dst rank =
    line#draw ctx ~src ~dst rank;
    scatter#draw ctx ~src ~dst rank

  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end


(** {2 Bubble dataset} ****************************************)


class bubble_dataset
  ?(glyph=Circle_glyph) ?(color=black)
  ?(min_radius=0.01) ?(max_radius=0.1) ?name triples =
  (** For plotting data with three values: x, y and z.  The result
      plots points at their x, y location as a scatter plot would however
      the z values are shown by changing the radius of the point. *)
object (self)
  inherit dataset ?name ()

  val triples = (triples : (point * float) list)

  method dimensions =
    let pts = List.map fst triples in
      points_rectangle pts


  method private z_value_range =
    (** [z_value_range] is the minimum and maximum z value of all
	triples.  This is used for determining the radius of a
	point. *)
    List.fold_left (fun (min, max) (_, z) ->
		      let min' = if z < min then z else min
		      and max' = if z > max then z else max
		      in min', max')
      (infinity, neg_infinity) triples


  method private radius ~min_z ~max_z vl =
    (** [compute_radius ~min_z ~max_z vl] gets the radius of the
	point. *)
    scale_value ~min:min_z ~max:max_z ~min':min_radius ~max':max_radius ~vl


  method residue ctx ~src ~dst _ =
    (** [residue ctx ~src ~dst rank] if we were to plot this right now
	with the given [dst] rectangle, how far out-of-bounds will we
	go in each direction. *)
    let tr = transform ~src ~dst in
    let min_z, max_z = self#z_value_range in
      List.fold_left
	(fun r (pt, z) ->
	   let pt' = tr pt in
	     if rectangle_contains dst pt'
	     then begin
	       let radius = self#radius ~min_z ~max_z z
	       in rectangle_max r (point_residue dst pt' radius)
	     end else r)
	zero_rectangle triples


  method draw ctx ~src ~dst _ =
    let tr = transform ~src ~dst in
    let min_z, max_z = self#z_value_range in
      List.iter (fun (pt, z) ->
		   let radius = self#radius ~min_z ~max_z z in
		   let pt' = tr pt in
		     if rectangle_contains src pt
		     then draw_point ctx ~color radius glyph pt')
	triples


  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end
