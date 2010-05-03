(** Dealing with plot legends.

    @author eaburns
    @since 2010-05-03
*)

open Drawing
open Geometry

type text_location =
  | Text_before
  | Text_after


type location =
  | At of text_location * float * float
  | Upper_left
  | Lower_left
  | Upper_right
  | Lower_right


let line_spacing = 0.01
  (** The spacing between lines in the legend. *)

let dimensions style ctx datasets =
  (** [dimensions style ctx datasets] gets the dimensions of a legend
      for the given datasets. *)
  List.fold_left
    (fun ((w, h) as dims) ds -> match ds#name with
       | None -> dims
       | Some txt ->
	   let txt_w, txt_h = text_dimensions ctx ~style txt in
	   let ico_w, ico_h = ds#legend_dimensions ctx in
	   let width = txt_w +. ico_w +. line_spacing in
	   let height = (max txt_h ico_h) +. line_spacing in
	     max width w, height +. h)
    (0., 0.) datasets


let locate ctx style dst datasets = function
    (** [locate ctx style dst datasest legend_loc] gets the location
	for drawing the plot legend of the given datasets. *)
  | At (txt_loc, x, y) ->
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
      in txt_loc, x_loc, y_loc



let draw ctx text_loc style datasets =
  (** [draw_legend ctx text_loc style datasets] draws the legend into
      the upper right corner of the unit square. *)
  let text_width, icon_width =
    List.fold_left (fun ((tw, iw) as ws) ds -> match ds#name with
		      | None -> ws
		      | Some txt ->
			  let tw', _ = text_dimensions ctx ~style txt in
			  let iw', _ = ds#legend_dimensions ctx in
			    max tw tw', max iw iw')
      (0., 0.) datasets
  in
  let width = text_width +. icon_width +. line_spacing in
    ignore (List.fold_left
	      (fun y_top ds -> match ds#name with
		 | None -> y_top
		 | Some txt ->
		     let tw, th = text_dimensions ctx ~style txt in
		     let iw, ih = ds#legend_dimensions ctx in
		     let height = if ih > th then ih else th in
		     let y = y_top +. (height /. 2.) in
		     let tx, ix = match text_loc with
		       | Text_before ->
			   (width -. line_spacing -. icon_width -. (tw /. 2.),
			    width -. (icon_width /. 2.))
		       | Text_after ->
			   (icon_width +. line_spacing +. (tw /. 2.),
			    icon_width /. 2.)
		     in
		       draw_text ctx ~style tx y txt;
		       ds#draw_legend ctx ~x:ix ~y;
		       y_top +. height +. line_spacing;)
	      0. datasets)
