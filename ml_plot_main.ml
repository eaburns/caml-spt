(** The main function for the ml-plot program.

    For now this is just for testing ml-plot, but eventually it should
    be a full program that reads plots from a file (or stdin) and will
    build the plot to the output.

    @author eaburns
    @since 2010-04-15
*)

open Geometry
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

    (* Test some text rotations. *)
    set_text_style ctx default_text_style;
    drawf ctx ~angle:~-.90. 0.5 0.5 "hello";
    drawf ctx 0.5 0.5 "hello";

    (* Test drawing fixed-width text. *)
    let txt = "rectangle is a long word" in
    let w, x, y = 0.12, 0.25, 0.25 in
    let h = fixed_width_text_height ctx w txt in
    let r = rectangle (x -. w /. 2.) (x +. w /. 2.) y (y +. h) in
      draw_fixed_width_text ctx ~x ~y ~width:w txt;
      draw_rectangle ctx r;


      (* Test clipped lines. *)
      let box = rectangle 0.6 0.7 0.6 0.8 in
	draw_rectangle ctx box;
	draw_line ctx ~style:default_line_style ~box
	  [ point 0.0 0.75;
	    point 0.6 0.75;
	    point 0.65 0.65;
	    point 0.9 0.55;
	  ];

	(* Test drawing points. *)
	let radius = 0.05 in
(*
	  draw_points ctx radius Cross_glyph [ point 0.8 0.1 ];
	  draw_points ctx radius Plus_glyph [ point 0.8 0.1 ];
	  draw_points ctx radius (Char_glyph 'M') [ point 0.8 0.1 ];
	  draw_points ctx radius Box_glyph [ point 0.8 0.1 ];
*)
	  draw_points ctx radius Triangle_glyph [ point 0.8 0.1 ];
	  draw_points ctx radius Ring_glyph [ point 0.8 0.1 ];


	Cairo_png.surface_write_to_file surface "test.png"

let _ = main ()
