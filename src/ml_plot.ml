(** The plot hierarchy.

    @author eaburns
    @since 2010-04-23
*)

open Geometry
open Drawing

let default_tick_style =
  (** The default style for the text associated with tick marks on a
      numeric axis. *)
  {
    text_font = "Palatino-Roman";
    text_size = 0.03;
    text_slant = Cairo.FONT_SLANT_NORMAL;
    text_weight = Cairo.FONT_WEIGHT_NORMAL;
    text_color = black;
  }

let default_label_style =
  (** The default style for the x and y axis labels and the title
      text. *)
  {
    text_font = "Palatino-Roman";
    text_size = 0.04;
    text_slant = Cairo.FONT_SLANT_NORMAL;
    text_weight = Cairo.FONT_WEIGHT_NORMAL;
    text_color = black;
  }


let text_padding = 0.01
  (** Padding around text *)


let white_background ctx width height =
  (** [white_background ctx width height] sets the background to
      white. *)
  fill_rectangle ctx ~color:white
    (rectangle 0. (float width) 0. (float height))


let draw_plot_to_gtk_area plot area =
  (** [draw_plot plot area] draws the plot to a GTK drawing area. *)
  let ctx = Cairo_lablgtk.create area#misc#window in
  let { Gtk.width = width ; Gtk.height = height } = area#misc#allocation in
  let sizef = float (min width height) in
    white_background ctx width height;
    (* Scale so that drawing can take place between 0. and 1. *)
    Cairo.scale ctx sizef sizef;
    plot#draw ctx;
    true


class virtual plot title =
  (** [plot title] a plot has a method for drawing. *)
object (self)

  method private title = match title with
    | Some t -> t
    | None -> "<not title>"

  method display =
    (** [display] opens a lablgtk window showing the plot. *)
    let init_size = 400 in
    let title = self#title and width = init_size and height = init_size in
    let w = GWindow.window ~title ~width ~height () in
    let area = GMisc.drawing_area ~width ~height ~packing:w#add() in
    let draw = draw_plot_to_gtk_area self in
      area#misc#realize ();
      ignore (area#event#connect#expose (fun _ -> draw area));
      ignore (w#connect#destroy GMain.quit);
      ignore (draw area);
      w#show();
      GMain.main ()


  method virtual draw : context -> unit
    (** [draw ctx] displays the plot to the given drawing
	context. *)
end
