(**

    @author eaburns
    @since 2010-04-19
*)

(** {6 Color} ****************************************)

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


(** {6 Text} ****************************************)

class text =
  fun
    ?(font = "Palatino-Roman")
    ?(size = 0.03)
    ?(slant = Cairo.FONT_SLANT_NORMAL)
    ?(weight = Cairo.FONT_WEIGHT_NORMAL)
    ?(color = black)
    string ->
object (self)
  val string = string
  val font = font
  val size = size
  val slant = slant
  val weight = weight
  val color = color

  method private set_style ctx =
    (** [set_stlye ctx] sets the text style. *)
    set_color ctx color;
    Cairo.select_font_face ctx font slant weight;
    Cairo.set_font_size ctx size


  method display ?(angle=0.) x y ctx =
    (** [display ?angle x y ctx] displays the text at the given center
	point. *)
    self#set_style ctx;
    let te = Cairo.text_extents ctx string in
    let w = te.Cairo.text_width and h = te.Cairo.text_height in
    let x_offs = te.Cairo.x_bearing and y_offs = te.Cairo.y_bearing in
      Cairo.save ctx;
      Cairo.move_to ctx 0. 0.;
      Cairo.translate ctx x y;
      Cairo.rotate ctx (angle *. (Math.pi /. 180.));
      Cairo.move_to ctx ~-.((w /. 2.) +. x_offs) ~-.((h /. 2.) +. y_offs);
      Cairo.show_text ctx string;
      Cairo.restore ctx


  method dimensions ctx =
    (** [dimensions ctx] gets the dimensions of the text.  *)
    self#set_style ctx;
    let te = Cairo.text_extents ctx string in
      te.Cairo.text_width, te.Cairo.text_height

end
