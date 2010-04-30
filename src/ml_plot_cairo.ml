(** A bunch of calls to cairo - Currently just for saving files *)

let as_png surface filename =
  Cairo_png.surface_write_to_file surface filename
