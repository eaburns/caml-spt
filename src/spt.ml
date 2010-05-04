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
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


let default_legend_style =
  (** The default style for legend text. *)
  {
    text_font = "Palatino-Roman";
    text_size = 0.03;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }

let default_label_style =
  (** The default style for the x and y axis labels and the title
      text. *)
  {
    text_font = "Palatino-Roman";
    text_size = 0.04;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


let text_padding = 0.02
  (** Padding around text *)


let output_centimeters = 8.
  (** The default size of the output. *)


class virtual plot title =
  (** [plot title] a plot has a method for drawing. *)
object (self)

  method private title = match title with
    | Some t -> t
    | None -> "<not title>"


  method display =
    (** [display] opens a lablgtk window showing the plot. *)
    Spt_gtk.create_display self self#title


  method virtual draw :
    suggested_width:float -> suggested_height:float -> context -> unit
    (** [draw ~suggested_width ~suggested_height ctx] displays the
	plot to the given drawing context. [width] and [height] are
	the width and height of the image after the caller scales
	it. *)


  method output filename =
    (** [output] saves the plot to a filename.  The type is pulled from
	the name, so you must include an extension *)
    Spt_cairo.save output_centimeters output_centimeters self filename

end
