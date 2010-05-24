(**

    @author jtd7
    @since 2010-05-21
   Contains a bulk of the style, cloor, etc factories that the
   various plots are going to make use of

*)

open Drawing

let make_glyph_factory glyph_set () =
  (** [make_glyph_factory glyph_set ()] makes a glyph factory which
      returns a new glyph at each call. *)
  let next = ref 0 in
  let n = Array.length glyph_set in
    (fun () ->
       let g = glyph_set.(!next) in
	 next := (!next + 1) mod n;
	 g)


let default_glyph_factory =
  (** [default_glyph_factory] gets the default glyph factory
      builder. *)
  let glyph_set =
    [| Circle_glyph;
       Ring_glyph;
       Triangle_glyph;
       Box_glyph;
       Square_glyph;
       Cross_glyph;
       Plus_glyph;
    |]
  in make_glyph_factory glyph_set


let numbered_glyph_factory =
  (** [numbered_glyph_factory] gets a glyph factory builder that
      returns numbers as the glyphs. *)
  let glyph_set =
    [| Char_glyph '0';
       Char_glyph '1';
       Char_glyph '2';
       Char_glyph '3';
       Char_glyph '4';
       Char_glyph '5';
       Char_glyph '6';
       Char_glyph '7';
       Char_glyph '8';
       Char_glyph '9';
    |]
  in make_glyph_factory glyph_set


let make_dash_factory dash_set () =
  (** [make_dash_factory dash_set ()] makes a dash pattern factory. *)
  let next = ref 0 in
  let n = Array.length dash_set in
    (fun () ->
       let d = dash_set.(!next) in
	 next := (!next + 1) mod n;
	 d)

let default_dash_factory =
  (** [default_dash_factory] gets the default dash factory builder. *)
  let default_dash_set =
    [|
      [| |];
      [| Length.Pt 6.; Length.Pt 2.; |];
      [| Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 1.; Length.Pt 1.; |];
      [| Length.Pt 5.; Length.Pt 2.; Length.Pt 1.; Length.Pt 2.; |];
      [| Length.Pt 10.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.;
	 Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 10.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 5.; Length.Pt 2.; Length.Pt 5.; Length.Pt 2.;
	 Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 4.; Length.Pt 2.; Length.Pt 4.; Length.Pt 1.;
	 Length.Pt 1.; Length.Pt 1.; Length.Pt 1.; Length.Pt 1.;
	 Length.Pt 1.; Length.Pt 1.; |];
    |]
  in make_dash_factory default_dash_set


let make_color_factory color_list =
  let colors = ref color_list in
    (fun () ->
       match !colors with
	   hd::tl -> (colors := tl @ [hd];
		      hd)
	 | _ -> failwith "Couldn't get next color")


let default_color_factory () =
  make_color_factory
    [black; red; green; blue; gray; purple; fuchsia; lavender; mustard]


(* EOF *)
