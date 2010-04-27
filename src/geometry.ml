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


type scale = {
  min : float;
  max : float;
}


let pi = 3.1415926535
  (** The constant pi. *)

let point ~x ~y = { x = x; y = y }
  (** [point ~x ~y] makes a new point *)


let scale ~min ~max = { min = min; max = max }
  (** [scale ~min ~max] creates a new scale. *)


let rectangle ~x_min ~x_max ~y_min ~y_max =
  (** [rectangle ~x_min ~x_max ~y_min ~y_max] creates a new rectangle. *)
  {
    x_min = x_min;
    x_max = x_max;
    y_min = y_min;
    y_max = y_max;
  }



let zero_rectangle = rectangle 0. 0. 0. 0.
  (** A rectangle with no dimensions. *)


let scale_value ~src ~dst vl =
  (** [scale_value ~src ~dst vl] converts [vl] from the initial scale
      to the new scale. *)
  let min = src.min and min' = dst.min in
  let diff = src.max -. min and diff' = dst.max -. min' in
  let s = diff' /. diff in
    ((vl -. min) *. s) +. min'


let xscale rect = scale rect.x_min rect.x_max
  (** [xscale rect] gets the scale of the x values from the rectangle. *)


let yscale rect = scale rect.y_min rect.y_max
  (** [yscale rect] gets the scale of the y values from the rectangle. *)


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


let face_forward rect =
  (** [face_forward rect] if the rectangle is facing the wrong
      direction, it is faced forward. *)
  if rect.x_min <= rect.x_max && rect.y_min <= rect.y_max
  then rect
  else
    let x_min, x_max = (if rect.x_min > rect.x_max
			then rect.x_max, rect.x_min
			else rect.x_min, rect.x_max)
    and y_min, y_max = (if rect.y_min > rect.y_max
			then rect.y_max, rect.y_min
			else rect.y_min, rect.y_max)
    in rectangle ~x_min ~x_max ~y_min ~y_max


let rectangle_extremes r0 r1 =
  (** [rectangle_extremes r0 r1] get the rectangle that contains both
      [r0] and [r1]. *)
  let x_min = if r0.x_min < r1.x_min then r0.x_min else r1.x_min
  and x_max = if r0.x_max > r1.x_max then r0.x_max else r1.x_max
  and y_min = if r0.y_min < r1.y_min then r0.y_min else r1.y_min
  and y_max = if r0.y_max > r1.y_max then r0.y_max else r1.y_max
  in rectangle ~x_min ~x_max ~y_min ~y_max


let rectangle_max r0 r1 =
  (** [rectangle_max r0 r1] takes the max of each dimension. *)
  rectangle
    ~x_min:(max r0.x_min r1.x_min)
    ~x_max:(max r0.x_max r1.x_max)
    ~y_min:(max r0.y_min r1.y_min)
    ~y_max:(max r0.y_max r1.y_max)


let rectangle_contains box p =
  (** [rectangle_contains box p] tests if the given point is within the
      rectangle. *)
  let box = face_forward box in
  let x = p.x and y = p.y in
    x >= box.x_min && x <= box.x_max && y >= box.y_min && y <= box.y_max


let points_rectangle pts =
  (** [points_rectangle pts] gets the rectangle enclosing a set of
      points. *)
  let x_min, x_max, y_min, y_max =
    Array.fold_left (fun (x_min, x_max, y_min, y_max) pt ->
		      let x = pt.x and y = pt.y in
		      let x_min' = if x < x_min then x else x_min
		      and x_max' = if x > x_max then x else x_max
		      and y_min' = if y < y_min then y else y_min
		      and y_max' = if y > y_max then y else y_max
		      in x_min', x_max', y_min', y_max')
      (infinity, neg_infinity, infinity, neg_infinity) pts
  in rectangle ~x_min ~x_max ~y_min ~y_max


let point_residual dst pt radius =
  (** [point_residual dst pt radius] gets the amount that the point
      will draw over the edge of the destination rectangle in each
      direction. *)
  let x = pt.x and y = pt.y in
  let x_min = x -. radius and x_max = x +. radius in
  let y_min = y +. radius and y_max = y -. radius in
  let x_min' = if x_min < dst.x_min then dst.x_min -. x_min else 0.
  and x_max' = if x_max > dst.x_max then x_max -. dst.x_max else 0.
  and y_min' = if y_min > dst.y_min then y_min -. dst.y_min else 0.
  and y_max' = if y_max < dst.y_max then dst.y_max -. y_max else 0.
  in rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max'


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
  let box = face_forward box in
  let p0_in = rectangle_contains box p0
  and p1_in = rectangle_contains box p1 in
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
