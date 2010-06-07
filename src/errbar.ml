(** Drawing of error bars.

    @author eaburns
    @since 2010-04-28
*)

open Geometry
open Drawing


let errbar_line_style =
  (** The line style for an error bar. *)
  {
    line_color = black;
    line_width = Length.Pt 0.75;
    line_dashes = [| |];
  }

let errbar_cap_size = Length.Pt 3.
  (** The size of the cap on an error bar. *)


let residual_vert ctx up ~src_y ~dst_x ~x ~y ~mag =
  (** [residual_vert ctx up ~src_y ~dst_x ~x ~y ~mag] computes the
      residual for a vertical error bar.  The [x] coordinate is in the
      destination coordinate system and the [y] coordinate is in the
      data coordinate system.  [src_y] and [dst_x] are the range of
      the source and destination y-axis and x-axis values
      respectively.  This assumes that the center point for the error
      bar is within the destination rectangle. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let y1 = if up then y +. mag else y -. mag in
  let clip = if up then y1 > src_y.max else y1 < src_y.min in
    if clip
    then zero_rectangle
    else begin
      let x0 = x -. errbar_cap_size and x1 = x +. errbar_cap_size in
      let res_min = if x0 < dst_x.min then dst_x.min -. x0 else 0.
      and res_max = if x1 > dst_x.max then x1 -. dst_x.max else 0. in
	{ zero_rectangle with x_min = res_min; x_max = res_max }
    end


let residual_horiz ctx left ~src_x ~dst_y ~x ~y ~mag =
  (** [residual_horiz ctx up ~src_x ~dst_y ~x ~y ~mag] computes the
      residual for a horizontal error bar.  The [x] coordinate is in
      the destination coordinate system and the [y] coordinate is in
      the data coordinate system.  [src_x] and [dst_y] are the range
      of the source and destination x-axis and y-axis values
      respectively.  This assumes that the center point for the error
      bar is within the destination rectangle. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let x1 = if left then x -. mag else x +. mag in
  let clip = if left then x1 < src_x.min else x1 > src_x.max in
    if clip
    then zero_rectangle
    else begin
      let y0 = y -. errbar_cap_size and y1 = y +. errbar_cap_size in
      let y_min = if y0 > dst_y.min then y0 -. dst_y.min else 0.
      and y_max = if y1 < dst_y.max then dst_y.max -. y1 else 0. in
	{ zero_rectangle with y_min = y_min; y_max = y_max }
    end


let draw_up ctx ?(style=errbar_line_style) ~src ~dst ~x ~y ~mag =
  (** [draw_up ctx ?style ~src ~dst ~x ~y ~mag] draws the top half of
      a vertical error bar.  The [x] coordinate is in the destination
      coordinate system and the [y] coordinate is in the data
      coordinate system.  [src] and [dst] are the range of the source
      and destination y-axis values.  This assumes that the center
      point for the error bar is within the destination rectangle. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let tr = range_transform ~src ~dst in
  let y1 = y +. mag in
  let y1_clipped = y1 > src.max || y1 < src.min in
    if y <= src.max && y1 >= src.min
    then begin
      let y0' = tr y and y1 = y +. mag in
      let y1' = if y1 > src.max then tr src.max else tr y1 in
	draw_line ctx ~style [point x y0'; point x y1'];
	if not y1_clipped
	then draw_line ctx ~style [ point (x -. errbar_cap_size) y1';
				    point (x +. errbar_cap_size) y1' ]
    end


let draw_down ctx ?(style=errbar_line_style) ~src ~dst ~x ~y ~mag =
  (** [draw_down ctx ?style ~src ~dst ~x ~y ~mag] draws the bottom half
      of a vertical error bar.  The [x] coordinate is in the
      destination coordinate system and the [y] coordinate is in the
      data coordinate system.  [src] and [dst] are the range of the
      source and destination y-axis values.  This assumes that the
      center point for the error bar is within the destination
      rectangle. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let tr = range_transform ~src ~dst in
  let y1 = y -. mag in
  let y1_clipped = y1 > src.max || y1 < src.min in
    if y >= src.min && y1 < src.max
    then begin
      let y0' = tr y  in
      let y1' = if y1 < src.min then tr src.min else tr y1 in
	draw_line ctx ~style [point x y0'; point x y1'];
	if not y1_clipped
	then draw_line ctx ~style [ point (x -. errbar_cap_size) y1';
				    point (x +. errbar_cap_size) y1' ]
    end


let draw_left ctx ?(style=errbar_line_style) ~src ~dst ~x ~y ~mag =
  (** [draw_left ctx ?style ~src ~dst ~x ~y ~mag] draws the top half of
      a vertical error bar.  The [y] coordinate is in the destination
      coordinate system and the [x] coordinate is in the data
      coordinate system.  [src] and [dst] are the range of the source
      and destination x-axis values.  This assumes that the center
      point for the error bar is within the destination rectangle. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let tr = range_transform ~src ~dst in
  let x0' = tr x and x1 = x -. mag in
  let x1', clip =
    if x1 < src.min then tr src.min, true else tr x1, false
  in
    draw_line ctx ~style [point x0' y; point x1' y];
    if not clip
    then begin
      draw_line ctx ~style [ point x1' (y -. errbar_cap_size);
			     point x1' (y +. errbar_cap_size) ]
    end


let draw_right ctx ?(style=errbar_line_style) ~src ~dst ~x ~y ~mag =
  (** [draw_right ctx ?style ~src ~dst ~x ~y ~mag] draws the top half
      of a vertical error bar.  The [y] coordinate is in the
      destination coordinate system and the [x] coordinate is in the
      data coordinate system.  [src] and [dst] are the range of the
      source and destination x-axis values.  This assumes that the
      center point for the error bar is within the destination
      rectangle. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let tr = range_transform ~src ~dst in
  let x0' = tr x and x1 = x +. mag in
  let x1', clip =
    if x1 > src.max then tr src.max, true else tr x1, false
  in
    draw_line ctx ~style [point x0' y; point x1' y];
    if not clip
    then begin
      draw_line ctx ~style [ point x1' (y -. errbar_cap_size);
			     point x1' (y +. errbar_cap_size) ]
    end
