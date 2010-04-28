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
    line_width = 0.002;
    line_dashes = [| |];
  }

let errbar_cap_size = 0.008
  (** The size of the cap on an error bar. *)


let draw_up ctx ?(style=errbar_line_style) ~src ~dst ~x ~y mag =
  (** [draw_up ctx ?style ~src ~dst ~x ~y mag] draws the top half of a
      vertical error bar.  The [x] coordinate is in the destination
      coordinate system and the [y] coordinate is in the data
      coordinate system.  [src] and [dst] are the range of the source
      and destination y-axis values.  This assumes that the center
      point for the error bar is within the destination rectangle. *)
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


let draw_down ctx ?(style=errbar_line_style) ~src ~dst ~x ~y mag =
  (** [draw_down ctx ?style ~src ~dst ~x ~y mag] draws the bottom half
      of a vertical error bar.  The [x] coordinate is in the
      destination coordinate system and the [y] coordinate is in the
      data coordinate system.  [src] and [dst] are the range of the
      source and destination y-axis values.  This assumes that the
      center point for the error bar is within the destination
      rectangle. *)
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


let draw_left ctx ?(style=errbar_line_style) ~src ~dst ~x ~y mag =
  (** [draw_left ctx ?style ~src ~dst ~x ~y mag] draws the top half of
      a vertical error bar.  The [y] coordinate is in the destination
      coordinate system and the [x] coordinate is in the data
      coordinate system.  [src] and [dst] are the range of the source
      and destination x-axis values.  This assumes that the center
      point for the error bar is within the destination rectangle. *)
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


let draw_right ctx ?(style=errbar_line_style) ~src ~dst ~x ~y mag =
  (** [draw_right ctx ?style ~src ~dst ~x ~y mag] draws the top half
      of a vertical error bar.  The [y] coordinate is in the
      destination coordinate system and the [x] coordinate is in the
      data coordinate system.  [src] and [dst] are the range of the
      source and destination x-axis values.  This assumes that the
      center point for the error bar is within the destination
      rectangle. *)
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
