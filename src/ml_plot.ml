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


class virtual plot title =
  (** [plot title] a plot has a method for drawing. *)
object (self)


  method private title = match title with
    | Some t -> t
    | None -> "<not title>"


  method display =
    (** [display] opens a lablgtk window showing the plot. *)
    Ml_plot_gtk.create_display self self#title


  method virtual draw : context -> unit
    (** [draw ctx] displays the plot to the given drawing
	context. *)
end
