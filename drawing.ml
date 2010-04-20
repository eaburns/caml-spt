(** Some basic Cairo drawing primitives.

    @author eaburns
    @since 2010-04-19
*)

(** {1 Color} ****************************************)

type color = { r : float; g : float; b : float; a : float}

let black = { r = 0.; g = 0.; b = 0.; a = 1.; }
  (** The color black. *)

let white = { r = 1.; g = 1.; b = 1.; a = 1.; }
  (** The color white. *)

let red = { r = 1.; g = 0.; b = 0.; a = 1.; }
  (** The color red. *)

let green = { r = 0.; g = 1.; b = 0.; a = 1.; }
  (** The color green. *)

let blue = { r = 0.; g = 0.; b = 1.; a = 1.; }
  (** The color blue. *)

let set_color ctx color =
  (** [set_color ctx color] sets the current color. *)
  Cairo.set_source_rgba ctx color.r color.g color.b color.a


(** {1 Text} ****************************************)

type text_style = {
  font : string;
  size : float;
  slant : Cairo.font_slant;
  weight : Cairo.font_weight;
  color : color;
}

let default_font_style =
  (** The default style for fonts. *)
  {
    font = "Palatino-Roman";
    size = 0.03;
    slant = Cairo.FONT_SLANT_NORMAL;
    weight = Cairo.FONT_WEIGHT_NORMAL;
    color = black;
  }

let set_font_style style ctx =
  (** [set_font_stlye style ctx] sets the text style. *)
  set_color ctx style.color;
  Cairo.select_font_face ctx style.font style.slant style.weight;
  Cairo.set_font_size ctx style.size


let display_string ?(style=default_font_style) ?(angle=0.) x y ctx str =
  (** [display_string ?style ?angle x y ctx str] displays the text at
      the given center point. *)
  set_font_style style ctx;
  let te = Cairo.text_extents ctx str in
  let w = te.Cairo.text_width and h = te.Cairo.text_height in
  let x_offs = te.Cairo.x_bearing and y_offs = te.Cairo.y_bearing in
    Cairo.save ctx;
    Cairo.move_to ctx 0. 0.;
    Cairo.translate ctx x y;
    Cairo.rotate ctx (angle *. (Math.pi /. 180.));
    Cairo.move_to ctx ~-.((w /. 2.) +. x_offs) ~-.((h /. 2.) +. y_offs);
    Cairo.show_text ctx str;
    Cairo.restore ctx


let string_dimensions ?(style=default_font_style) ctx str =
  (** [string_dimensions ?style ctx str] gets the dimensions of the
      text.  *)
  set_font_style style ctx;
  let te = Cairo.text_extents ctx str in
    te.Cairo.text_width, te.Cairo.text_height


(** {2 Formatted text} ****************************************)

let displayf ?(style=default_font_style) ?(angle=0.) x y ctx fmt =
  (** [displayf ?style ?angle x y ctx fmt] displays the formatted text
      at the given center point. *)
  Printf.kprintf (display_string ~style ~angle x y ctx) fmt


let dimensionsf ?(style=default_font_style) ctx fmt =
  (** [dimensionsf ?style ctx fmt] gets the dimensions of the
      formatted text and returns the string.  *)
  let str_and_dims str =
    let w, h = string_dimensions ~style ctx str in
      str, w, h
  in Printf.kprintf str_and_dims fmt


