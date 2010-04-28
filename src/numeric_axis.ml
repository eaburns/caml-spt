(** Drawing of a numeric axis.

    @author eaburns
    @since 2010-04-24
*)

open Printf
open Geometry
open Drawing

let axis_style =
  (** The line style of a numeric axis. *)
  {
    line_color = black;
    line_dashes = [| |];
    line_width = 0.0025;
  }


let tick_length = 0.02
  (** The length of a tick mark. *)


let tick_style =
  (** The line style of a tick mark. *)
  {
    line_color = black;
    line_dashes = [| |];
    line_width = 0.0012;
  }


let axis_padding = 0.05
  (** Padding between the axis and the data. *)


(** {1 Tick marks} ****************************************)

let tick_locations rng =
  (** [tick_locations rng] computes the location of tick marks on a
      numeric axis with the given range. *)
  let min = rng.min and max = rng.max in
  let tick major vl = vl, if major then Some (sprintf "%.2f" vl) else None in
    [ tick true min;
      tick false (min +. ((max -. min) *. 0.25));
      tick true (min +. ((max -. min) *. 0.50));
      tick false (min +. ((max -. min) *. 0.75));
      tick true max; ]


let max_tick_text_width ctx style ticks =
  (** [max_tick_text_width ctx style ticks] gets the maximum width of
      the text for the given ticks. *)
  List.fold_left (fun m (_, txt_opt) ->
		    match txt_opt with
		      | None -> m
		      | Some txt ->
			  let w = fst (text_dimensions ctx ~style txt) in
			    if w > m then w else m)
    0. ticks

let max_tick_text_height ctx style ticks =
  (** [max_tick_text_height ctx style ticks] gets the maximum height of
      the text for the given ticks. *)
  List.fold_left (fun m (_, txt_opt) ->
		    match txt_opt with
		      | None -> m
		      | Some txt ->
			  let h = snd (text_dimensions ctx ~style txt) in
			    if h > m then h else m)
    0. ticks


(** {1 Drawing an x-axis} ****************************************)

let resize_for_x_axis
    ctx ~label_style ~tick_style ~pad ~y_min ~src ~dst label ticks =
  (** [resize_for_x_axis ctx ~label_style ~tick_style ~pad ~y_min ~src
      ~dst label ticks] gets the new scale after making room for the
      x-axis tick marks and label.  [pad] is the padding between text.
      The result is a new (y_min * x_max) that will have room for the
      x-axis and the x-tick label text. *)
  let label_room =
    match label with
      | None -> 0.
      | Some label -> snd (text_dimensions ctx ~style:label_style label)
  in
  let ticks_with_text = List.filter (fun (_, t) -> t <> None) ticks in
  let x_tick_txt_height =
    match ticks_with_text with
      | (_,  Some txt) :: tl -> snd (text_dimensions ctx ~style:tick_style txt)
      | _ -> 0. in
  let y_min' =
    y_min
    -. label_room -. pad
    -. tick_length -. pad -. x_tick_txt_height
    -. axis_padding
  in
  let over (vl, txt_opt) =
    match txt_opt with
      | None -> 0.
      | Some txt ->
	  let width, _ = text_dimensions ctx ~style:tick_style txt in
	  let tr = range_transform ~src ~dst in
	  let x = tr vl in
	  let over = (x +. (width /. 2.)) -. dst.max in
	    if over > 0. then over else 0.
  in
  let max_over =
    (* Check how far each tick text on the x-axis goes over the
       maximum x-value so that this can be pushed back to fit the
       text. *)
    List.fold_left (fun max tick ->
		      let o = over tick in
			if o > max then o else max)
      0.ticks
  in
    y_min', dst.max -. max_over


let draw_x_tick ctx style ~pad ~y scale (vl, t_opt) =
  (** [draw_x_tick ctx style ~pad ~y scale t] draws an x-tick with the
      top at the given [y] location.. *)
  let x = scale vl in
  let len = if t_opt = None then tick_length /. 2. else tick_length in
    draw_line ctx ~style:tick_style [ point x y; point x (y +. len) ];
    begin match t_opt with
      | Some txt ->
	  draw_text_centered_below ctx ~style x (y +. len +. pad) txt;
      | None -> ()
    end


let draw_x_axis
    ctx ~tick_style ~label_style ~pad ~y ~src ~dst label ticks =
  (** [draw_x_axis ctx ~tick_style ~label_style ~pad ~y ~src ~dst
      label ticks] draws an x-axis. [scale] is a function that
      converts an x-value in the original data coordinates to the
      destination x-coordinate system.  [y] is the bottom y-coordinate
      of the axis label. *)
  let tick_text_height = max_tick_text_height ctx tick_style ticks in
  let tr = range_transform ~src ~dst in
  let h = match label with
    | None -> 0.
    | Some label ->
	let h = snd (text_dimensions ctx ~style:label_style label) in
	  draw_text_centered_above ctx 0.5 y label;
	  h
  in
  let y' = y -. h -. pad -. tick_text_height -. pad -. tick_length in
    List.iter (draw_x_tick ctx tick_style ~pad ~y:y' tr) ticks;
    draw_line ctx ~style:axis_style [ point dst.min y'; point dst.max y'; ]


(** {1 Drawing a y-axis} ****************************************)

let resize_for_y_axis ctx ~label_style ~tick_style ~pad ~x_min label ticks =
  (** [resize_for_y_axis ctx ~label_style ~tick_style ~pad ~x_min
      label ticks] gets the new minimum and maximum x-values after
      making room for the y-axis tick marks and label.  [pad] is the
      padding between text. *)
  let label_room =
    match label with
      | None -> 0.
      | Some label -> snd (text_dimensions ctx ~style:label_style label)
  in
  let tick_text_width = max_tick_text_width ctx tick_style ticks in
    x_min
    +. label_room +. pad
    +. tick_length +. pad +. tick_text_width
    +. axis_padding


let draw_y_tick ctx style ~pad ~x tr (vl, t_opt) =
  (** [draw_y_tick ctx style ~pad ~x tr t]
      draws a y-tick with the left at the given [x] location.. *)
  let y = tr vl in
  let len = if t_opt = None then tick_length /. 2. else tick_length in
    draw_line ctx ~style:tick_style [ point x y; point (x -. len) y ];
    begin match t_opt with
      | Some txt ->
	  draw_text_centered_before ctx ~style (x -. len -. pad) y txt;
      | None -> ()
    end


let draw_y_axis
    ctx ~tick_style ~label_style ~pad ~x ~src ~dst label ticks =
  (** [draw_y_axis ctx ~tick_style ~label_style ~pad ~x ~src ~dst
      label ticks] draws a y-axis. [x] is the left x-coordinate of the
      axis label. *)
  let tick_text_width = max_tick_text_width ctx tick_style ticks in
  let tr = range_transform ~src ~dst in
  let h = match label with
    | None -> 0.
    | Some label ->
	let h = snd (text_dimensions ctx ~style:label_style label) in
	  draw_text ctx ~angle:270. (x +. h /. 2.) 0.5 label;
	  h
  in
  let x' = x +. h +. pad +. tick_text_width +. pad +. tick_length in
    List.iter (draw_y_tick ctx tick_style ~pad ~x:x' tr) ticks;
    draw_line ctx ~style:axis_style [ point x' dst.min; point x' dst.max; ]
