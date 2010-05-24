(**

    @author eaburns
    @since 2010-04-28
*)


open Geometry
open Drawing


class virtual errbar_dataset triples =
  (** A dataset that consists of a bunch of error bars. *)
object
  inherit Num_by_num_dataset.dataset ()

  val triples = (triples : triple array)
    (* point and magnitude. *)
end

(** {1 Vertical error bars} ****************************************)

class vertical_errbar_dataset ?color triples =
  (** A set of vertical error bars. *)
  let style = match color with
    | None -> Errbar.errbar_line_style
    | Some color -> { Errbar.errbar_line_style with line_color = color }
  in
object (self)
  inherit errbar_dataset triples

  method residual ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      Array.fold_left
	(fun r t ->
	   let pt = tr (point t.i t.j) in
	   let x = pt.x and y = pt.y and mag = t.k in
	   let up_residue = Errbar.residual_up ctx dst ~x ~y ~mag
	   and down_residue = Errbar.residual_down ctx dst ~x ~y ~mag in
	     rectangle_extremes r (rectangle_extremes up_residue down_residue))
	zero_rectangle triples


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


  method draw ctx ~src ~dst =
    (** [draw ctx ~src ~dst] draws the data to the plot. *)
    let tr = range_transform ~src:(xrange src) ~dst:(xrange dst) in
      Array.iter (fun t ->
		    if rectangle_contains src (point t.i t.j)
		    then begin
		      let src = yrange src and dst = yrange dst in
		      let x = tr t.i and y = t.j and mag = t.k in
			Errbar.draw_up ctx ~style ~src ~dst ~x ~y ~mag;
			Errbar.draw_down ctx ~style ~src ~dst ~x ~y ~mag;
		    end)
	triples


  method draw_legend _ ~x:_ ~y:_ = ()

  method legend_dimensions _ = 0., 0.

  method avg_slope = nan

end


(** {1 Horizontal error bars} ****************************************)

class horizontal_errbar_dataset ?color triples =
  (** A set of horizontal error bars. *)
  let style = match color with
    | None -> Errbar.errbar_line_style
    | Some color -> { Errbar.errbar_line_style with line_color = color }
  in
object (self)
  inherit errbar_dataset triples

  method residual ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      Array.fold_left
	(fun r t ->
	   let pt = tr (point t.i t.j) in
	   let x = pt.x and y = pt.y and mag = t.k in
	   let left_residue = Errbar.residual_left ctx dst ~x ~y ~mag
	   and right_residue = Errbar.residual_right ctx dst ~x ~y ~mag in
	     rectangle_extremes r (rectangle_extremes
				     left_residue right_residue))
	zero_rectangle triples


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


  method draw ctx ~src ~dst =
    (** [draw ctx ~src ~dst] draws the data to the plot. *)
    let tr = range_transform ~src:(yrange src) ~dst:(yrange dst) in
      Array.iter (fun t ->
		    if rectangle_contains src (point t.i t.j)
		    then begin
		      let src = xrange src and dst = xrange dst in
		      let x = t.i and y = tr t.j and mag = t.k in
			Errbar.draw_left ctx ~style ~src ~dst ~x ~y ~mag;
			Errbar.draw_right ctx ~style ~src ~dst ~x ~y ~mag;
		    end)
	triples


  method draw_legend _ ~x:_ ~y:_ = ()

  method legend_dimensions _ = 0., 0.

  method avg_slope = nan

end

(* no api constructors, since no one would ever want to make these
   manually, they're always part of something else. *)

(* EOF *)
