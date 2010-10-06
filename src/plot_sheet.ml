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


let centered_on ~w ~h ~pad plot =
  (** [centered_on ~w ~h ~pad plot] centers [plot] on a sheet of size
      [w]x[h] with [pad] between the edges of the plot and the ends of the
      sheet. *)
  let plot = Oo.copy plot in
  let wpt = Length.as_pt w and hpt = Length.as_pt h in
  let ppt = Length.as_pt pad in
  let sheet =
    new plot_sheet [ (Length.Pt (wpt /. 2.), Length.Pt (hpt /. 2.), 0., plot) ]
  in
    sheet#set_size ~w:(Length.Pt wpt) ~h:(Length.Pt hpt);
    plot#set_size ~w:(Length.Pt (wpt -. 2. *. ppt))
      ~h:(Length.Pt (hpt -. 2. *. ppt));
    sheet


let us_letter ?(landscape=false) plot =
  (** [us_letter ?landscape plot] clone the given plot to a us letter
      sized sheet. *)
  let win = if landscape then 11. else 8.5 in
  let hin = if landscape then 8.5 else 11. in
    centered_on ~w:(Length.In win) ~h:(Length.In hin)
      ~pad:(Length.In 0.25) plot


let double_letter ?(landscape=false) plot =
  (** [us_letter ?landscape plot] clone the given plot to a double us letter
      sized sheet (11x17). *)
  let win = if landscape then 17. else 11. in
  let hin = if landscape then 11. else 17. in
    centered_on ~w:(Length.In win) ~h:(Length.In hin)
      ~pad:(Length.In 0.25) plot
