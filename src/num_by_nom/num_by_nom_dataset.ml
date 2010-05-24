(** Numeric by nominal datasets.

    @author eaburns
    @since 2010-05-21
*)

open Drawing
open Geometry

class virtual dataset name =
  (** [dataset name] is a dataset that is plottable on a nominal x
      axis and a numeric y axis. *)
object

  val name = (name : string)
    (** The name of the dataset is what appears on the x-axis. *)

  method virtual dimensions : range
    (** [dimensions] gets the min and maximum value from the
	dataset. *)


  method x_label_height : context -> text_style -> float -> float =
    (** [x_label_height context style width] is the height of the
	label on thesrc/ x-axis. *)
    (fun ctx style width -> fixed_width_text_height ctx ~style width name)


  method draw_x_label :
    context -> x:float -> y:float -> text_style -> width:float -> unit =
    (** [draw_x_label context ~x ~y style ~width] draws the x-axis
	label to the proper location. *)
    (fun ctx ~x ~y style ~width ->
       let half_width = width /. 2. in
       draw_fixed_width_text ctx ~x:(x +. half_width) ~y ~style ~width name)

  method virtual residual :
    context -> src:range -> dst:range -> width:float -> x:float -> range
    (** [residual ctx ~src ~dst width x] get a rectangle containing
	the maximum amount the dataset will draw off of the
	destination rectangle in each direction. *)

  method virtual draw :
    context -> src:range -> dst:range -> width:float -> x:float ->unit
    (** [draw ctx ~src ~dst width x] draws the dataset to the
	plot.  [x] is the left-hand-side x value. *)
end

(** {6 Grouped datasets} ****************************************)

let between_padding = Length.Pt 5.

class dataset_group group_name datasets =
object(self)

  inherit dataset group_name

  method private dataset_width ctx width =
    let n = float (List.length datasets) in
    let total_padding = (n -. 1.) *. (ctx.units between_padding) in
      if n > 1.
      then (width -. total_padding) /. n
      else width


  method private dataset_name_height ctx style width =
    (** [dataset_name_height ctx style width] gets the height of the
	dataset names on the x-axis. *)
    let ds_width = self#dataset_width ctx width in
      List.fold_left
	(fun h ds ->
	   let ht = ds#x_label_height ctx style ds_width in
	     if ht > h then ht else h)
	neg_infinity datasets

  method dimensions =
    List.fold_left (fun r ds -> range_extremes r ds#dimensions)
      (range ~min:infinity ~max:neg_infinity) datasets


  method x_label_height ctx style width =
    (self#dataset_name_height ctx style width)
    +. (fixed_width_text_height ctx ~style width group_name)


  method draw_x_label ctx ~x ~y style ~width =
    let between_padding = ctx.units between_padding in
    let ds_name_height = self#dataset_name_height ctx style width in
    let ds_width = self#dataset_width ctx width in
    let center = x +. (width /. 2.) in
      ignore
	(List.fold_left
	   (fun x ds ->
	      ds#draw_x_label ctx ~x ~y style ~width:ds_width;
	      x +. ds_width +. between_padding)
	   x datasets);
      draw_fixed_width_text ctx ~x:center ~y:(y +. ds_name_height)
	~style ~width group_name


  method residual ctx ~src ~dst ~width ~x =
    let ds_width = self#dataset_width ctx width in
    let between_padding = ctx.units between_padding in
      fst (List.fold_left
	     (fun (r, x) ds ->
		(range_extremes r (ds#residual ctx ~src ~dst
				     ~width:ds_width ~x),
		 x +. ds_width +. between_padding))
	     ((range ~min:infinity ~max:neg_infinity), x) datasets)


  method draw ctx ~src ~dst ~width ~x =
    let ds_width = self#dataset_width ctx width in
    let between_padding = ctx.units between_padding in
      ignore (List.fold_left
		(fun x ds ->
		   ds#draw ctx ~src ~dst ~width:ds_width ~x;
		   x +. ds_width +. between_padding)
		x datasets)
end