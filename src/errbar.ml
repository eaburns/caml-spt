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


let residual_up ctx dst ~x ~y ~mag =
  (** [residual_up ctx dst ~x ~y ~mag] computes the residual. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let y = y -. mag in
  let x_min = x -. errbar_cap_size and x_max = x +. errbar_cap_size in
  let x_min' = if x_min < dst.x_min then dst.x_min -. x_min else 0.
  and x_max' = if x_max > dst.x_max then x_max -. dst.x_max else 0.
  and y_min' = if y > dst.y_min then y -. dst.y_min else 0.
  and y_max' = if y < dst.y_max then dst.y_max -. y else 0.
  in rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max'


let residual_down ctx dst ~x ~y ~mag =
  (** [residual_down ctx dst ~x ~y ~mag] computes the residual. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let y = y +. mag in
  let x_min = x -. errbar_cap_size and x_max = x +. errbar_cap_size in
  let x_min' = if x_min < dst.x_min then dst.x_min -. x_min else 0.
  and x_max' = if x_max > dst.x_max then x_max -. dst.x_max else 0.
  and y_min' = if y > dst.y_min then y -. dst.y_min else 0.
  and y_max' = if y < dst.y_max then dst.y_max -. y else 0.
  in rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max'


let residual_left ctx dst ~x ~y ~mag =
  (** [residual_left ctx dst ~x ~y ~mag] computes the residual. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let x = x -. mag in
  let y_min = y +. errbar_cap_size and y_max = y -. errbar_cap_size in
  let x_min' = if x < dst.x_min then dst.x_min -. x else 0.
  and x_max' = if x > dst.x_max then x -. dst.x_max else 0.
  and y_min' = if y_min > dst.y_min then y_min -. dst.y_min else 0.
  and y_max' = if y_max < dst.y_max then dst.y_max -. y_max else 0.
  in rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max'


let residual_right ctx dst ~x ~y ~mag =
  (** [residual_right ctx dst ~x ~y ~mag] computes the residual. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let x = x +. mag in
  let y_min = y +. errbar_cap_size and y_max = y -. errbar_cap_size in
  let x_min' = if x < dst.x_min then dst.x_min -. x else 0.
  and x_max' = if x > dst.x_max then x -. dst.x_max else 0.
  and y_min' = if y_min > dst.y_min then y_min -. dst.y_min else 0.
  and y_max' = if y_max < dst.y_max then dst.y_max -. y_max else 0.
  in rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max'


let draw_up ctx ?(style=errbar_line_style) ~src ~dst ~x ~y ~mag =
  (** [draw_up ctx ?style ~src ~dst ~x ~y ~mag] draws the top half of
      a vertical error bar.  The [x] coordinate is in the destination
      coordinate system and the [y] coordinate is in the data
      coordinate system.  [src] and [dst] are the range of the source
      and destination y-axis values.  This assumes that the center
      point for the error bar is within the destination rectangle. *)
  let errbar_cap_size = ctx.units errbar_cap_size in
  let tr = range_transform ~src ~dst in
  let y0' = tr y and y1 = y +. mag in
  let y1', clip =
    if y1 > src.max then tr src.max, true else tr y1, false
  in
    draw_line ctx ~style [point x y0'; point x y1'];
    if not clip
    then begin
      draw_line ctx ~style [ point (x -. errbar_cap_size) y1';
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
  let y0' = tr y and y1 = y -. mag in
  let y1', clip =
    if y1 < src.min then tr src.min, true else tr y1, false
  in
    draw_line ctx ~style [point x y0'; point x y1'];
    if not clip
    then begin
      draw_line ctx ~style [ point (x -. errbar_cap_size) y1';
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
