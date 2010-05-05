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

let default_width = Sizing.Cm 8.
  (** The default plot width in centimeters. *)

let default_height = Sizing.Cm 8.
  (** The default plot height in centimeters. *)


class virtual plot title =
  (** [plot title] a plot has a method for drawing. *)
object (self)

  val mutable width = default_width

  method width = width

  val mutable height = default_height

  method height = width

  method aspect_ratio =
    (** [aspect_ratio] gets the current aspect ratio of the plot. *)
    assert (Sizing.same_type width height);
    (let w = Sizing.measure_to_float width
     and h = Sizing.measure_to_float height in
       if w > h then 1., h /. w else w /. h, 1.)


  method set_size ~w ~h =
    (** [set_size ~w ~h] resizes the plot. *)
    assert (Sizing.same_type w h);
    width <- w;
    height <- h


  method private title = match title with
    | Some t -> t
    | None -> "<no title>"


  method display =
    (** [display] opens a lablgtk window showing the plot. *)
    Spt_gtk.create_display self self#title


  method output filename =
    (** [output filename] saves the plot to a filename.  The type is
	pulled from the name, so you must include an extension *)
    Spt_cairo.save width height self filename


  method virtual draw : context -> unit
    (** [draw ctx] displays the plot to the given drawing context with
	the given aspect ratio.  It is up to the caller to scale this
	to the appropriate width/height. *)
end
