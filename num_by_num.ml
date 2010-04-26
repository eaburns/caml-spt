(** Numeric by numeric datasets.

    TODO: The scatter dataset should make room for the glyph radius
    when it says its dimensions.

    @author eaburns
    @since 2010-04-25
*)

open Geometry
open Drawing
open Ml_plot


(** {1 Points datasets} ****************************************)

class virtual points_dataset ?name points =
  (** A dataset composed of a set of points. *)
object
  inherit num_by_num_dataset ?name ()

  val points = (points : point list)
    (** The list of points. *)

  method dimensions = points_rectangle points
    (** [dimensions] gets the rectangle around the points. *)
end


(** {2 Scatter dataset} ****************************************)


let amount_over dst pt radius =
  (** [amount_over dst pt radius] gets the amount that the point will
      draw over the edge of the destination rectangle in each
      direction. *)
  let x = pt.x and y = pt.y in
  let x_min = x -. radius and x_max = x +. radius in
  let y_min = y +. radius and y_max = y -. radius in
  let x_min' = if x_min < dst.x_min then dst.x_min -. x_min else 0.
  and x_max' = if x_max > dst.x_max then x_max -. dst.x_max else 0.
  and y_min' = if y_min > dst.y_min then y_min -. dst.y_min else 0.
  and y_max' = if y_max < dst.y_max then dst.y_max -. y_max else 0.
  in rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max'


let glyphs =
  (** The default glyphs for scatter plots. *)
  [| Circle_glyph; Ring_glyph; Cross_glyph; Plus_glyph;
     Square_glyph; Box_glyph; Triangle_glyph |]


let cur_glyph = ref 0
  (** The index into [glyphs] that the next scatter plot will use by
      default. *)


class scatter_dataset ?glyph ?(color=black) ?(radius=0.01) ?name points =
  (** A scatter plot dataset. *)
object
  inherit points_dataset ?name points

  val glyph =
    (** The glyph to use for this dataset. *)
    match glyph with
      | None ->
	  let i = !cur_glyph in
	    cur_glyph := (!cur_glyph + 1) mod (Array.length glyphs);
	    glyphs.(i)
      | Some g -> g


  method max_over ctx ~src ~dst =
    let tr = transform ~src ~dst in
      List.fold_left
	(fun r pt ->
	   if rectangle_contains src pt
	   then rectangle_max r (amount_over dst (tr pt) radius)
	   else r)
	(rectangle 0. 0. 0. 0.) points


  method draw ctx ~src ~dst _ =
    let tr = transform ~src ~dst in
    let pts = List.map tr (List.filter (rectangle_contains src) points) in
      draw_points ctx ~color radius glyph pts

  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end

(** {2 Line dataset} ****************************************)


let dashes =
  (** The dash patterns for lines. *)
  [|
    [| |]; [| 0.01; 0.01 |];
  |]


let cur_dashes = ref 0
  (** The current dash pattern will be assigned to the next line. *)


class line_dataset ?dash_pattern ?(width=0.002) ?(color=black) ?name points =
  (** A line plot dataset. *)
object
  inherit points_dataset ?name points

  val style =
    (** The style of the line *)
    {
      line_color = color;
      line_dashes = begin match dash_pattern with
	| None ->
	    let i = !cur_dashes in
	      cur_dashes := (!cur_dashes + 1) mod (Array.length dashes);
	      dashes.(i)
	| Some d -> d
      end;
      line_width = width;
    }

  method max_over _ ~src:_ ~dst = rectangle 0. 0. 0. 0.

  method draw ctx ~src ~dst _ =
    let tr = transform ~src ~dst in
    let pts = List.map tr points in
      draw_line ctx ~box:dst ~style pts

  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end


(** {1 Line points dataset} ****************************************)


class line_points_dataset
  ?dash_pattern ?width ?glyph ?radius ?color
  ?name points =
  (** A line with points plot dataset. *)
object
  inherit num_by_num_dataset ?name ()

  val line = new line_dataset ?dash_pattern ?width ?color points
  val scatter = new scatter_dataset ?glyph ?radius ?color points

  method dimensions = rectangle_extremes scatter#dimensions line#dimensions

  method max_over ctx ~src ~dst =
    rectangle_extremes
      (line#max_over ctx ~src ~dst)
      (scatter#max_over ctx ~src ~dst)

  method draw ctx ~src ~dst rank =
    line#draw ctx ~src ~dst rank;
    scatter#draw ctx ~src ~dst rank

  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end


(** {1 Bubble dataset} ****************************************)


class bubble_dataset
  ?(glyph=Circle_glyph) ?(color=black) ?(max_radius=0.1) ?name triples =
  (** A line with points plot dataset. *)
object
  inherit num_by_num_dataset ?name ()

  val triples = (triples : (point * float) list)

  method dimensions =
    let pts = List.map fst triples in
      points_rectangle pts


  method max_over ctx ~src ~dst =
    let tr = transform ~src ~dst in
    let max_z =
      List.fold_left (fun m (_, z) -> if z > m then z else m)
	neg_infinity triples
    in
      List.fold_left
	(fun r (pt, z) ->
	   let pt' = tr pt in
	     if rectangle_contains dst pt'
	     then begin
	       let radius = max_radius *. (z /. max_z)
	       in rectangle_max r (amount_over dst pt' radius)
	     end else r)
	(rectangle 0. 0. 0. 0.) triples


  method draw ctx ~src ~dst rank =
    let tr = transform ~src ~dst in
    let max_z =
      List.fold_left (fun m (_, z) -> if z > m then z else m)
	neg_infinity triples
    in
      List.iter (fun (pt, z) ->
		   let radius = max_radius *. (z /. max_z) in
		   let pt' = tr pt in
		     if rectangle_contains src pt
		     then draw_point ctx ~color radius glyph pt')
	triples


  method draw_legend_entry ctx ~x ~y = failwith "Unimplemented"
end
