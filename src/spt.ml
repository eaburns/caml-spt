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
    text_font = "Courier";
    text_size = Length.Pt 8.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


let default_legend_style =
  (** The default style for legend text. *)
  {
    text_font = "Courier";
    text_size = Length.Pt 10.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }

let default_label_style =
  (** The default style for the x and y axis labels and the title
      text. *)
  {
    text_font = "Courier";
    text_size = Length.Pt 12.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


let text_padding = Length.Pt 4.
  (** Padding around text *)


let default_width = Length.Cm 8.
  (** The default plot width in centimeters. *)

let default_height = Length.Cm 8.
  (** The default plot height in centimeters. *)


class virtual plot title =
  (** [plot title] a plot has a method for drawing. *)
object (self)

  val mutable width = default_width

  method width = width

  val mutable height = default_height

  method height = height


  method private size ctx =
    (** [size ctx] gets the size of the plot in the units of the
	context. *)
    (ctx.units width), (ctx.units height)


  method set_size ~w ~h =
    (** [set_size ~w ~h] resizes the plot. *)
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
    Spt_cairo.save self filename


  method virtual draw : context -> unit
    (** [draw ctx] displays the plot to the given drawing context with
	the given aspect ratio.  It is up to the caller to scale this
	to the appropriate width/height. *)
end

(* EOF *)
