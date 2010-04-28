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


(** {1 Errorbar dataset} ****************************************)

class virtual errbar_dataset triples =
  (** A dataset that consists of a bunch of error bars. *)
object
  inherit Num_by_num_dataset.dataset ()

  val triples = (triples : triple array)
    (* point and magnitude. *)
end

(** {2 Vertical error bars} ****************************************)

class vertical_errbar_dataset triples =
  (** A set of vertical error bars. *)
object (self)
  inherit errbar_dataset triples

  method dimensions =
    Array.fold_left
      (fun r t ->
	 let low = t.j -. t.k and high = t.j +. t.k in
	 let x = t.i in
	 let rect = rectangle ~x_min:x ~x_max:x ~y_min:low ~y_max:high
	 in rectangle_extremes r rect)
      (rectangle
	 ~x_min:infinity ~x_max:neg_infinity
	 ~y_min:infinity ~y_max:neg_infinity)
      triples


  method residual ctx ~src ~dst rank = zero_rectangle

  method draw ctx ~src ~dst rank =
    (** [draw ctx ~src ~dst rank] draws the data to the plot. *)
    let tr = range_transform ~src:(xrange src) ~dst:(xrange dst) in
      Array.iter (fun t ->
		    if rectangle_contains src (point t.i t.j)
		    then begin
		      let src = yrange src and dst = yrange dst in
		      let x = tr t.i and y = t.j and mag = t.k in
			draw_up ctx ~src ~dst ~x ~y mag;
			draw_down ctx ~src ~dst ~x ~y mag;
		    end)
	triples


  method draw_legend_entry _ ~x ~y _ = failwith "Unimplemented"

end


(** {2 Horizontal error bars} ****************************************)

class horizontal_errbar_dataset triples =
  (** A set of horizontal error bars. *)
object
  inherit errbar_dataset triples

  method dimensions =
    Array.fold_left
      (fun r t ->
	 let low = t.i -. t.k and high = t.i +. t.k in
	 let y = t.j in
	 let rect = rectangle ~x_min:low ~x_max:high ~y_min:y ~y_max:y
	 in rectangle_extremes r rect)
      (rectangle
	 ~x_min:infinity ~x_max:neg_infinity
	 ~y_min:infinity ~y_max:neg_infinity)
      triples


  method residual ctx ~src ~dst rank = zero_rectangle

  method draw ctx ~src ~dst rank =
    (** [draw ctx ~src ~dst rank] draws the data to the plot. *)
    let tr = range_transform ~src:(yrange src) ~dst:(yrange dst) in
      Array.iter (fun t ->
		    if rectangle_contains src (point t.i t.j)
		    then begin
		      let src = xrange src and dst = xrange dst in
		      let x = t.i and y = tr t.j and mag = t.k in
			draw_left ctx ~src ~dst ~x ~y mag;
			draw_right ctx ~src ~dst ~x ~y mag;
		    end)
	triples


  method draw_legend_entry _ ~x ~y _ = failwith "Unimplemented"

end
