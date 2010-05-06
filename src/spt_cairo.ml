(** A bunch of calls to cairo - Currently just for saving files *)


type files =
  | Postscript
  | PNG
  | PDF
  | Unknown of string


let resize context plot width height =
  (* Scale so that drawing can take place between 0. and 1. *)
  let w,h = match width, height with
    | Length.Pt w, Length.Pt h -> w,h
    | Length.Px w, Length.Px h -> (float w),(float h)
    | _, _-> assert false in
    (plot#set_size ~w:width ~h:height;
     let x_ratio, y_ratio = plot#aspect_ratio in
       Drawing.fill_rectangle context ~color:Drawing.white
	 (Geometry.rectangle 0. w 0. h);
       Drawing.scale context (w /. x_ratio) (h /. y_ratio))


(* saving functionality *)
let as_png width height plot filename =
  let width_px = Length.as_px width and height_px = Length.as_px height in
  let surface = (Cairo.image_surface_create
		   Cairo.FORMAT_ARGB32 ~width:width_px ~height:height_px) in
  let context = Cairo.create surface in
    resize context plot width height;
    plot#draw context;
    Cairo_png.surface_write_to_file surface filename


let as_ps width height plot filename =
  let width_pt = Length.as_pt width and height_pt = Length.as_pt height in
  let chan = open_out filename in
  let surface = (Cairo_ps.surface_create_for_channel chan
		   ~width_in_points:width_pt ~height_in_points:height_pt) in
  let context = Cairo.create surface in
    resize context plot width height;
    plot#draw context;
    Cairo.surface_finish surface;
    close_out chan


let as_pdf width height plot filename =
  let width_pt = Length.as_pt width and height_pt = Length.as_pt height in
  let chan = open_out filename in
  let surface = (Cairo_pdf.surface_create_for_channel chan
		   ~width_in_points:width_pt ~height_in_points:height_pt) in
  let context = Cairo.create surface in
    resize context plot width height;
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
