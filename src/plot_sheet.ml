(** A sheet of plots.

    @author eaburns
    @since 2010-09-08
*)

open Drawing
open Geometry

class type plot_type =
  object
    method draw : Drawing.context -> unit
    method height : Length.t
    method width : Length.t
  end


let plot_size ctx theta plot =
  (** [plot_size ctx theta plot] gets the width and height of the plot
      when rotated at the given angle. *)
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Geometry.rotated_size ~theta ~w ~h


let draw_plot ctx plot ~x ~y ~theta =
  (** [draw_plot ctx plot ~x ~y ~theta] draws the given plot centered
      at [x],[y].  The return value is the width and height of the
      bounding box around the given plot.  [theta] is in radians. *)
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Drawing.save_transforms ctx;
    Drawing.translate ctx x y;
    Drawing.rotate ctx theta;
    Drawing.translate ctx ~-.(w /. 2.) ~-.(h /. 2.);
    plot#draw ctx;
    Drawing.draw_rectangle ~style:Drawing.default_line_style ctx
      (rectangle 0. w 0. h);
    Drawing.restore_transforms ctx;
    let w, h = Geometry.rotated_size ~theta ~w ~h in
      Drawing.draw_rectangle ~style:Drawing.default_line_style ctx
	(rectangle (x -. w /. 2.) (x +. w /. 2.)
	   (y -. h /. 2.) (y +. h /. 2.));
      w, h


class plot_sheet plot =
object(self)

  inherit Spt.plot None

  val angle = ref Geometry.pi

  method draw ctx =
    let angle = (let a = !angle in angle := a +. 0.1; a) in
    let width, height = self#size ctx in
    let cx = width /. 2. and cy = height /. 2. in
      ignore (draw_plot ctx plot ~x:cx ~y:cy ~theta:angle)

end
