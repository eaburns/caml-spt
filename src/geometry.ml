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


type triple = {
  i : float;
  j : float;
  k : float;
}

type line = point list

type range = {
  min : float;
  max : float;
}


let pi = 3.1415926535
  (** The constant pi. *)


let point ~x ~y = { x = x; y = y }
  (** [point ~x ~y] makes a new point *)


let triple ~i ~j ~k = { i = i; j = j; k = k }
  (** [triple ~i ~j ~k] makes a 3-dimensional point. *)


let range ~min ~max = { min = min; max = max }
  (** [range ~min ~max] creates a new range. *)


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


let rectangle_to_slope r =
  (** Calculates the slope associated with a rectangle *)
  let dx = r.x_max -. r.x_min
  and dy = r.y_max -. r.y_min in
    dy /. dx

let range_transform ~src ~dst vl =
  (** [range_transform ~src ~dst vl] converts [vl] from the initial scale
      to the new scale. *)
  let min = src.min and min' = dst.min in
  let diff = src.max -. min and diff' = dst.max -. min' in
  let s = diff' /. diff in
    ((vl -. min) *. s) +. min'


let range_scale ~src ~dst vl =
  (** [range_scale ~src ~dst vl] scales the value to the new range. *)
  let d = abs_float (dst.max -. dst.min)
  and s = abs_float (src.max -. src.min)
  in vl *. (d /. s)


let range_extremes a b =
  (** [range_extremes a b] gets the extremes of the two ranges. *)
  let max = if a.max > b.max then a.max else b.max in
  let min = if a.min < b.min then a.min else b.min in
    range ~min ~max


let range_max a b =
  (** [range_max a b] get a new range with the maximum min and max
      values over a and b. *)
  let min' = if a.min > b.min then a.min else b.min
  and max' = if a.max > b.max then a.max else b.max
  in range min' max'


let xrange rect = range rect.x_min rect.x_max
  (** [xrange rect] gets the range of the x values from the rectangle. *)


let yrange rect = range rect.y_min rect.y_max
  (** [yrange rect] gets the range of the y values from the rectangle. *)


let point_transform ~src ~dst =
  (** [rectangle_transform ~src ~dst pt] transforms a point drawn on
      the [src] rectangle to a point on the [dst] rectangle. *)
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


let rectangle_residual dst r =
  (** [rectangle_residual dst r] gets the residual of [r] on [dst].
      (how much does [r] go over the extremes of [dst] on each side.
      (assumes the destination rectangle has a greater y_min than
      y_max.) *)
  let x_min = if r.x_min < dst.x_min then dst.x_min -. r.x_min else 0.
  and x_max = if r.x_max > dst.x_max then r.x_max -. dst.x_max else 0.
  and y_min = if r.y_min > dst.y_min then r.y_min -. dst.y_min else 0.
  and y_max = if r.y_max < dst.y_max then dst.y_max -. r.y_max else 0.
  in rectangle ~x_min ~x_max ~y_min ~y_max



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


let interpolate line x =
  (** [interpolate line x] interpolates a y-coordinate for the given
      [x] coordinate on the given line.  [line] is an array of
      points. *)
  let rec do_interp line ~n ~x i =
    let pt = line.(i) in
    let pt_x = pt.x in
      if pt_x = x
      then pt.y
      else
	if pt_x > x || i = n - 1
	then begin
	  let pt0, pt1 =
	    if i = 0
	    then line.(i), line.(i + 1)
	    else line.(i - 1), line.(i)
	  in
	  let x0 = pt0.x and y0 = pt0.y in
	  let x1 = pt1.x and y1 = pt1.y in
	  let deltax = x1 -. x0 in
	  let m = if deltax = 0. then nan else (y1 -. y0) /. (x1 -. x0) in
	  let b = ~-. m *. x0 +. y0 in
	    x *. m +. b
	end else do_interp line ~n ~x (i + 1)
  in
  let n = Array.length line in
    if n < 2 then invalid_arg "Geometry.interpolate: need at least two points";
    do_interp line ~n ~x 0


(** {1 Clipping} ****************************************)

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
      let deltax = x1 -. x0 in
      let m = if deltax = 0. then nan else (y1 -. y0) /. (x1 -. x0) in
      let b = ~-. m *. x0 +. y0 in
      let f x = x *. m +. b in
      let f_inv y = if m <> m then x1 else (y -. b) /. m in
      let p0 = (if not p0_in
		then clip_point_on_line box f f_inv p0
		else p0)
      and p1 = (if not p1_in
		then clip_point_on_line box f f_inv p1
		else p1)
      in p0, p1
    end else p0, p1


(*** Conversions ***********************************************)

let point_of_tuple (a,b) =
  { x  = a; y = b; }

let points_of_tuples ab_list =
  List.map point_of_tuple ab_list

let point_of_list lst =
  match lst with
      [a; b] -> { x = a; y = b; }
    | _ -> failwith ("Geometry.point_of_list: Didn't get a two element list")


let point_of_array ar =
  match ar with
    | [| a; b|] -> { x = a; y = b; }
    | _ -> failwith ("Geometry.point_of_array: Didn't get a two element array")


let triple_of_threeple (a,b,c) =
  { i  = a; j = b; k = c; }


let triple_of_list lst =
  match lst with
      [a; b; c] -> { i  = a; j = b; k = c; }
    | _ -> failwith ("Geometry.triple_of_list: Didn't get a two element list")


let triple_of_array ar =
  match ar with
    | [| a; b; c|] -> { i  = a; j = b; k = c; }
    | _ -> failwith ("Geometry.triple_of_array: Didn't get a two element array")


let line_of_tuples ab_list =
  ((List.map point_of_tuple ab_list) :> line)


let line_of_lists ab_list_list =
  ((List.map point_of_list ab_list_list) :> line)


let line_of_arrays ab_array_list =
  ((List.map point_of_array ab_array_list) :> line)


let line_of_points point_list =
  (point_list :> line)

(* EOF *)
