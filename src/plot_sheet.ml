(** A sheet of plots.

    @author eaburns
    @since 2010-09-08
*)

open Drawing
open Geometry


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
    Drawing.restore_transforms ctx


class plot_sheet lplots =
  (** [plat_sheet lplots] is a simple class that draws plots at a given
      x,y and theta.  [lplots] is a list of [located_plot]s. *)
object(self)

  inherit Spt.plot None

  val angle = ref Geometry.pi

  method draw ctx =
    let units = ctx.units in
      List.iter (fun (xlen, ylen, theta, p) ->
		   let x = units xlen and y = units ylen in
		     draw_plot ctx p ~x ~y ~theta)
	lplots

end


let us_letter ?(landscape=false) plot =
  (** [us_letter ?landscape plot] clone the given plot to a us letter
      sized sheet. *)
  let plot = Oo.copy plot in
  let xin = if landscape then 11. /. 2. else 8.5 /. 2. in
  let yin = if landscape then 8.5 /. 2. else 11. /. 2. in
  let sheet = new plot_sheet [ (Length.In xin, Length.In yin, 0., plot) ] in
    if landscape
    then sheet#set_size ~w:(Length.In 11.) ~h:(Length.In 8.5)
    else sheet#set_size ~w:(Length.In 8.5) ~h:(Length.In 11.);
    if landscape
    then plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.)
    else plot#set_size ~w:(Length.In 8.) ~h:(Length.In 10.5);
    sheet
