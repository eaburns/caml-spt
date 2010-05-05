(** A bunch of calls to cairo - Currently just for saving files *)


type files =
  | Postscript
  | PNG
  | PDF
  | Unknown of string


let resize context plot width height =
  (* Scale so that drawing can take place between 0. and 1. *)
  let w,h = match width, height with
      Sizing.Pt w, Sizing.Pt h -> w,h
    | Sizing.Px w, Sizing.Px h -> (float w),(float h)
    | _, _-> assert false in
    (plot#set_size ~w:width ~h:height;
     let x_ratio, y_ratio = plot#aspect_ratio in
       Drawing.fill_rectangle context ~color:Drawing.white
	 (Geometry.rectangle 0. w 0. h);
       Drawing.scale context (w /. x_ratio) (h /. y_ratio))


(* saving functionality *)
let as_png width height plot filename =
  let width_px = Sizing.convert width Sizing.Pixels
  and height_px = Sizing.convert height Sizing.Pixels in
  let width_int = Sizing.pixels_to_int width_px
  and height_int = Sizing.pixels_to_int height_px in
  let surface = (Cairo.image_surface_create
		   Cairo.FORMAT_ARGB32
		   ~width:width_int
		   ~height:height_int) in
  let context = Cairo.create surface in
    resize context plot width_px height_px;
    plot#draw context;
    Cairo_png.surface_write_to_file surface filename


let as_ps width height plot filename =
  let width_pt = Sizing.convert width Sizing.Points
  and height_pt = Sizing.convert height Sizing.Points in
  let chan = open_out filename in
  let surface = (Cairo_ps.surface_create_for_channel chan
		   ~width_in_points:(Sizing.points_to_flt width_pt)
		   ~height_in_points:(Sizing.points_to_flt height_pt)) in
  let context = Cairo.create surface in
    resize context plot width_pt height_pt;
    plot#draw context;
    Cairo.surface_finish surface;
    close_out chan


let as_pdf width height plot filename =
  let width_pt = Sizing.convert width Sizing.Points
  and height_pt = Sizing.convert height Sizing.Points in
  let chan = open_out filename in
  let surface = (Cairo_pdf.surface_create_for_channel chan
		   ~width_in_points:(Sizing.points_to_flt width_pt)
		   ~height_in_points:(Sizing.points_to_flt height_pt)) in
  let context = Cairo.create surface in
    resize context plot width_pt height_pt;
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


let save ?width ?height plot filename =
  let width = (match width with
		 | None -> plot#width
		 | Some w -> w)
  and height = (match height with
		  | None -> plot#height
		  | Some h -> h) in
    match (filetype filename) with
      | Postscript -> as_ps width height plot filename
      | PNG -> as_png width height plot filename
      | PDF -> as_pdf width height plot filename
      | Unknown ext -> failwith ("Cannot save unknown filetype " ^ ext)



(* EOF *)
