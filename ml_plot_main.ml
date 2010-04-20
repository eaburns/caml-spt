(** The main function for the ml-plot program.

    For now this is just for testing ml-plot, but eventually it should
    be a full program that reads plots from a file (or stdin) and will
    build the plot to the output.

    @author eaburns
    @since 2010-04-15
*)

open Drawing

let main () =

  let size = 400 in
  let sizef = float size in
  let surface =
    Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width:size ~height:size
  in
  let ctx = Cairo.create surface in

    (* White background *)
    Drawing.set_color ctx Drawing.white;
    Cairo.rectangle ctx 0. 0. sizef sizef;
    Cairo.fill ctx;
    Drawing.set_color ctx Drawing.black;

    (* Scale so that drawing can take place between 0. and 1. *)
    Cairo.scale ctx sizef sizef;
    Cairo.set_line_width ctx (1. /. sizef);

    set_text_style ctx default_text_style;
    displayf ctx ~angle:~-.90. 0.5 0.5 "hello";
    displayf ctx 0.5 0.5 "hello";

    fixed_width_text ctx 0.25 0.25 0.2
      "aoeuaoeuaoeuaoeu hello there this   is some text";

    Cairo_png.surface_write_to_file surface "test.png"

let _ = main ()
