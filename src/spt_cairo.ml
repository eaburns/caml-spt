(** A bunch of calls to cairo - Currently just for saving files *)

let pixels_per_centimeter = (96. *. 2.54)
and points_per_centimeter = (72. *. 2.54)

let width = 400
and height = 400
and width_in_points = (float (72 * 4))
and height_in_points = (float (72 * 4))


type files =
  | Postscript
  | PNG
  | PDF
  | Unknown of string


(* resizing functionality and units conversion *)

let points_to_cm pval =
  (** converts a number of points into corresponding centimeters measure *)
  (float_of_int pval) /. points_per_centimeter


let points_to_in pval =
  (** converts a number of points into corresponding inches measure *)
  (float_of_int pval) /. (points_per_centimeter /. 2.54)


let pixels_to_cm pval =
  (** converts a number of points into corresponding centimeters measure *)
  (float_of_int pval) /. pixels_per_centimeter


let pixels_to_in pval =
  (** converts a number of points into corresponding inches measure *)
  (float_of_int pval) /. (pixels_per_centimeter /. 2.54)


let background_resize context width height =
  (* Resizes the surface that we're drawing upon.  Also adds in a white
      background *)
  let sizef = min width height in
    Drawing.fill_rectangle context ~color:Drawing.white
      (Geometry.rectangle 0. width 0. height);
    (* Scale so that drawing can take place between 0. and 1. *)
    Drawing.scale context sizef sizef


(* saving functionality *)

let as_png plot filename =
  let surface = (Cairo.image_surface_create
		   Cairo.FORMAT_ARGB32 ~width ~height) in
  let context = Cairo.create surface in
    background_resize context (float_of_int width) (float_of_int height);
    plot#draw context;
    Cairo_png.surface_write_to_file surface filename


let as_ps plot filename =
  let chan = open_out filename in
  let surface = (Cairo_ps.surface_create_for_channel chan
		   ~width_in_points ~height_in_points) in
  let context = Cairo.create surface in
    background_resize context width_in_points height_in_points;
    plot#draw context;
    Cairo.surface_finish surface;
    close_out chan


let as_pdf plot filename =
  let chan = open_out filename in
  let surface = (Cairo_pdf.surface_create_for_channel chan
		   ~width_in_points ~height_in_points) in
  let context = Cairo.create surface in
    background_resize context width_in_points height_in_points;
    plot#draw context;
    Cairo.surface_finish surface;
    close_out chan



(* determining filetype and saving *)

let filetype file =
  let file_split = Str.split (Str.regexp "\\.") file in
    match (List.rev file_split) with
	[] -> Unknown ""
      | ext::tl -> (match (String.lowercase ext) with
		      | "ps" -> Postscript
		      | "png" -> PNG
		      | "pdf" -> PDF
		      | _ -> Unknown ext)


let save plot filename =
  match (filetype filename) with
    | Postscript -> as_ps plot filename
    | PNG -> as_png plot filename
    | PDF -> as_pdf plot filename
    | Unknown ext -> failwith ("Cannot save unknown filetype " ^ ext)



(* EOF *)
