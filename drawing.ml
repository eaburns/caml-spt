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


  method private extents ctx =
    (** [extents ctx] gets the extents of the text. *)
    self#set_style ctx;
    Cairo.text_extents ctx string


  method private centered_x x ctx =
    (** [centered_x x ctx] gets the x coordinate that will center the
	text on the x-axis. *)
    let te = self#extents ctx in
      x -. (te.Cairo.text_width /. 2.) -. te.Cairo.x_bearing

  method private before_x x ctx =
    (** [before_x x ctx] gets the x coordinate that will place the
	text before the given x location. *)
    let te = self#extents ctx in
      x -. te.Cairo.text_width -. te.Cairo.x_bearing


  method private after_x x ctx =
    (** [after_x x ctx] gets the x coordinate that will place the
	text after the given x location. *)
    let te = self#extents ctx in
      x -. te.Cairo.x_bearing

  method private centered_y y ctx =
    (** [centered_y y ctx] gets the y coordinate that will center the
	text on the y-axis. *)
    let te = self#extents ctx in
      y -. (te.Cairo.text_height /. 2.) -. te.Cairo.y_bearing


  method private above_y y ctx =
    (** [above_y y ctx] gets the y coordinate that will place the text
	above the y location. *)
    let te = self#extents ctx in
      y +. te.Cairo.y_bearing +. te.Cairo.text_height


  method private below_y y ctx =
    (** [below_y y ctx] gets the y coordinate that will place the text
	below the y location. *)
    let te = self#extents ctx in
      y +. te.Cairo.y_bearing


  method private display angle x y ctx =
    (** [display angle x y ctx] displays the text. *)
    Cairo.save ctx;
    Cairo.move_to ctx x y;
    Cairo.rotate ctx (angle *. (Math.pi /. 180.));
    Cairo.show_text ctx string;
    Cairo.restore ctx


end
