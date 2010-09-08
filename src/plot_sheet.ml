(** A sheet of plots.

    @author eaburns
    @since 2010-09-08
*)

open Drawing
open Geometry

(*
class type plot_type =
  object
    method draw : Drawing.context -> unit
    method height : Length.t
    method width : Length.t
    method set_size : w:Length.t -> h:Length.t -> unit
  end
*)


let plot_size ctx theta plot =
  (** [plot_size ctx theta plot] gets the width and height of the plot
      when rotated at the given angle. *)
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Geometry.rotated_size ~theta ~w ~h


let draw_plot ctx plot ~x ~y ~theta =
  (** [draw_plot ctx plot ~x ~y ~theta] draws the given plot centered
      at [x],[y].  [theta] is in radians. *)
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Drawing.save_transforms ctx;
    Drawing.translate ctx x y;
    Drawing.rotate ctx theta;
    Drawing.translate ctx ~-.(w /. 2.) ~-.(h /. 2.);
    plot#draw ctx;
(*
    Drawing.draw_rectangle ~style:Drawing.default_line_style ctx
      (rectangle 0. w 0. h);
*)
    Drawing.restore_transforms ctx;
    ()
(*
    let w, h = Geometry.rotated_size ~theta ~w ~h in
      Drawing.draw_rectangle ~style:Drawing.default_line_style ctx
	(rectangle (x -. w /. 2.) (x +. w /. 2.)
	   (y -. h /. 2.) (y +. h /. 2.))
*)


type 'a located_plot =
  | Centered of float * 'a
  | Absolute of Length.t * Length.t * float * 'a


let draw_located_plot ~w ~h ctx plot =
  (** [draw_located_plot ~w ~h ctx plot] draws a 'located' plot. *)
  match plot with
    | Centered (t, p) ->
	let cx = w /. 2. and cy = h /. 2. in
	  draw_plot ctx p ~x:cx ~y:cy ~theta:t
    | Absolute (x, y, t, p) ->
	let units = ctx.units in
	let x = units x and y = units y in
	  draw_plot ctx p ~x ~y ~theta:t


class plot_sheet lplots =
  (** [plat_sheet lplots] is a simple class that draws plots at a given
      x,y and theta.  [lplots] is a list of [located_plot]s. *)
object(self)

  inherit Spt.plot None

  val angle = ref Geometry.pi

  method draw ctx =
    let w, h = self#size ctx in
    List.iter (draw_located_plot ~w ~h ctx) lplots

end


let us_letter ?(landscape=false) plot =
  let plot = Oo.copy plot in
  let theta = if landscape then 2. *. Geometry.pi else 0. in
  let sheet = new plot_sheet [ Centered (theta, plot) ] in
    sheet#set_size ~w:(Length.In 8.5) ~h:(Length.In 11.);
    if landscape
    then plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.)
    else plot#set_size ~w:(Length.In 8.) ~h:(Length.In 10.5);
    sheet
