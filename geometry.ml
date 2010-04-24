(** Some basic geometric primitives.

    Points are a good way to store 2 floats.

    Rectangles can be used for converting between translated and
    scaled coordinate systems and for clipping line segments.

    @author eaburns
    @since 2010-04-16
*)

type rectangle = {
  x_min : float;
  x_max : float;
  y_min : float;
  y_max : float;
}


type point = {
  x : float;
  y : float;
}


let point ~x ~y = { x = x; y = y }
  (** [point ~x ~y] makes a new point *)


let rectangle ~x_min ~x_max ~y_min ~y_max =
  (** [rectangle ~x_min ~x_max ~y_min ~y_max] creates a new rectangle. *)
  {
    x_min = x_min;
    x_max = x_max;
    y_min = y_min;
    y_max = y_max;
  }


let scale_value ~min ~max ~min' ~max' ~vl =
  (** [scale_value ~min ~max ~min' ~max' ~vl] converts [vl] from the
      initial scale to the new scale. *)
  let diff = max -. min and diff' = max' -. min' in
  let s = diff' /. diff in
    ((vl -. min) *. s) +. min'


let transform ~src ~dst =
  (** [transform ~src ~dst pt] transforms a point drawn on the [src]
      rectangle to a point on the [dst] rectangle. *)
  let src_x_min = src.x_min
  and dst_x_min = dst.x_min
  and src_y_min = src.y_min
  and dst_y_min = dst.y_min in
  let src_x_diff = src.x_max -. src_x_min
  and dst_x_diff = dst.x_max -. dst_x_min
  and src_y_diff = src.y_max -. src_y_min
  and dst_y_diff = dst.y_max -. dst_y_min in
  let x_scale = dst_x_diff /. src_x_diff
  and y_scale = dst_y_diff /. src_y_diff in
    (fun pt ->
       point
	 (((pt.x -. src_x_min) *. x_scale) +. dst_x_min)
	 (((pt.y -. src_y_min) *. y_scale) +. dst_y_min))


let contains box p =
  (** [contains box p] tests if the given point is within the
      rectangle. *)
  let x = p.x and y = p.y in
    x >= box.x_min && x <= box.x_max && y >= box.y_min && y <= box.y_max


let clip_point_on_line box f f_inv p =
  (** [clip_point_on_line box f f_inv p] clips a point to the box
      given that it resides on the given line. *)
  let x = p.x and y = p.y in
  let x', y' =
    if x > box.x_max
    then box.x_max, f box.x_max
    else
      if x < box.x_min
      then box.x_min, f box.x_min
      else x, y in
  let x'', y'' =
    if y' > box.y_max
    then f_inv box.y_max, box.y_max
    else
      if y' < box.y_min
      then f_inv box.y_min, box.y_min
      else x', y'
  in point x'' y''


let clip_line_segment box ~p0 ~p1 =
  (** [clip_line_segment box ~p0 ~p1] clips a line segment to the
      given bounding box. *)
  let p0_in = contains box p0 and p1_in = contains box p1 in
    if (not p0_in) || (not p1_in)
    then begin
      let x0 = p0.x
      and y0 = p0.y
      and x1 = p1.x
      and y1 = p1.y in
      let m = (y1 -. y0) /. (x1 -. x0) in
      let b = ~-. m *. x0 +. y0 in
      let f x = x *. m +. b in
      let f_inv y = (y -. b) /. m in
      let p0 = (if not p0_in
		then clip_point_on_line box f f_inv p0
		else p0)
      and p1 = (if not p1_in
		then clip_point_on_line box f f_inv p1
		else p1)
      in p0, p1
    end else p0, p1
