(** Dealing with plot legends.

    @author eaburns
    @since 2010-05-03
*)

open Drawing
open Geometry
open Verbosity

type text_location =
  | Text_before
  | Text_after


type location =
  | At of text_location * float * float
  | Upper_left
  | Lower_left
  | Upper_right
  | Lower_right


let padding = Length.Pt 4.
  (** Padding between legend text and the icons. *)


let max_height ctx style datasets =
  (** [max_height ctx style datasets] gets the max entry height. *)
  let text_height = font_suggested_line_height ~style ctx in
    List.fold_left
      (fun h ds ->
	 let _, icon_height = ds#legend_dimensions ctx in
	   max h (max text_height icon_height))
      0. datasets


let max_widths ctx style datasets =
  (** [max_widths ctx style datasets] gets the max entry height. *)
  List.fold_left
    (fun ((tw, iw) as dims) ds -> match ds#name with
       | None -> dims
       | Some txt ->
	   let txt_w, _ = text_dimensions ctx ~style txt in
	   let ico_w, _ = ds#legend_dimensions ctx in
	     max tw txt_w, max iw ico_w)
    (0., 0.) datasets


let dimensions style ctx datasets =
  (** [dimensions style ctx datasets] gets the dimensions of a legend
      for the given datasets. *)
  let padding = ctx.units padding in
  let ndatasets = float (List.length datasets) in
  let ent_height = max_height ctx style datasets in
  let text_width, icon_width = max_widths ctx style datasets
  in (text_width +. icon_width +. padding), ent_height *. ndatasets


let locate ctx style dst datasets = function
    (** [locate ctx style dst datasest legend_loc] gets the location
	for drawing the plot legend of the given datasets. *)
  | At (txt_loc, x, y) ->
      vprintf verb_debug "legend location: text %s icons (%f, %f)\n"
	(match txt_loc with Text_after -> "after" | _ -> "before")
	x y;
      txt_loc, x, y
  | loc ->
      let w, h = dimensions style ctx datasets in
      let txt_loc = match loc with
	| Upper_left | Lower_left -> Text_after
	| _ -> Text_before
      and x_loc = match loc with
	| Upper_left | Lower_left -> dst.x_min
	| _ -> dst.x_max -. w
      and y_loc = match loc with
	| Upper_left | Upper_right -> dst.y_max
	| _ -> dst.y_min -. h
      in
	vprintf verb_debug "legend location: text %s icons (%f, %f)\n"
	  (match txt_loc with Text_after -> "after" | _ -> "before")
	  x_loc y_loc;
	txt_loc, x_loc, y_loc



let draw ctx src text_loc style datasets =
  (** [draw_legend ctx src text_loc style datasets] draws the legend
      into the upper right corner of the unit square. *)
  let datasets =
    (* Sort the datasets in descending order. *)
    List.sort (fun a b ->
		 let ay, _ = a#mean_y_value src
		 and by, _ = b#mean_y_value src in
		   match classify_float ay, classify_float by with
		     | FP_nan, FP_nan -> 0
		     | _, FP_nan -> 1
		     | FP_nan, _ -> ~-1
		     | _, _ ->
			 if ay > by then ~-1 else if ay < by then 1 else 0)
      datasets
  in
  let padding = ctx.units padding in
  let text_width, icon_width = max_widths ctx style datasets in
  let width = text_width +. icon_width +. padding in
  let entry_height = max_height ctx style datasets in
  let _ (* height *) =
    List.fold_left
      (fun y_top ds -> match ds#name with
	 | None -> y_top
	 | Some txt ->
	     let tw, _ = text_dimensions ctx ~style txt in
	     let y = y_top +. (entry_height /. 2.) in
	     let tx, ix = match text_loc with
	       | Text_before ->
		   (width -. padding -. icon_width -. (tw /. 2.),
		    width -. (icon_width /. 2.))
	       | Text_after ->
		   (icon_width +. padding +. (tw /. 2.),
		    icon_width /. 2.)
	     in
	       draw_text_line ctx ~style ~center:tx ~top:y_top txt;
	       ds#draw_legend ctx ~x:ix ~y;
	       y_top +. entry_height)
      0. datasets in
    ()
