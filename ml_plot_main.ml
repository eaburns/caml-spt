(** The main function for the ml-plot program.

    For now this is just for testing ml-plot, but eventually it should
    be a full program that reads plots from a file (or stdin) and will
    build the plot to the output.

    @author eaburns
    @since 2010-04-15
*)

open Geometry
open Drawing
open Ml_plot

let nominal_plot () =
  new Num_by_nom.plot
    ~title:"Title text"
    ~ylabel:"Y label text"
    ~y_min:0.
    ~y_max:1.
    [
      (object
	 inherit Num_by_nom.dataset "Dataset one"
	 method dimensions = 0., 0.
	 method residual _ ~src ~dst _ _ = range nan nan
	 method draw _ ~src ~dst _ _ = ()
       end);
      (object
	 inherit Num_by_nom.dataset "This is example dataset two"
	 method dimensions = 0., 0.
	 method residual _ ~src ~dst _ _ = range nan nan
	 method draw _ ~src ~dst _ _ = ()
       end);
      (object
	 inherit Num_by_nom.dataset
	   "Some third dataset that has a very long name"
	 method dimensions = 0., 0.
	 method residual _ ~src ~dst _ _ = range nan nan
	 method draw _ ~src ~dst _ _ = ()
       end);
    ]


let numeric_plot () =
  let pts0 =
    [| point 0.5 6.0; point 1.3 2.0; point 7.3 8.1; point 3.8 0.05; |]
  and pts1 =
    [| point 9.0 1.117; point 4.1415926535 1.7; point 6.0 8.12; |]
  and pts2 =
    [| point 9.0 3.117; point 4.1415926535 3.7; point 6.0 3.12; |]
  and pts3 =
    [| point 10.5 6.0; point 3.3 2.0; point 2.3 8.1; point 9.8 0.05; |]
  in
  let next_dash = Num_by_num.default_dash_factory () in
  let next_glyph = Num_by_num.default_glyph_factory () in
    new Num_by_num.plot
      ~title:"Title text"
      ~xlabel:"X label text"
      ~ylabel:"Y label text"
      [
	new Num_by_num.composite_dataset ~name:"ds0" [
	  new Num_by_num.line_dataset ~name:"ds0" (next_dash ()) pts0;
	  new Num_by_num.scatter_dataset ~name:"ds0" (next_glyph ()) pts0;
	];
	new Num_by_num.composite_dataset ~name:"ds1" [
	  new Num_by_num.line_dataset ~name:"ds1" (next_dash ()) pts1;
	  new Num_by_num.scatter_dataset ~name:"ds1" (next_glyph ()) pts1;
	];
	new Num_by_num.composite_dataset ~name:"ds2" [
	  new Num_by_num.line_dataset ~name:"ds2" (next_dash ()) pts2;
	  new Num_by_num.scatter_dataset ~name:"ds2" (next_glyph ()) pts2;
	];
	new Num_by_num.composite_dataset ~name:"ds3" [
	  new Num_by_num.line_dataset ~name:"ds3" (next_dash ()) pts3;
	  new Num_by_num.scatter_dataset ~name:"ds3" (next_glyph ()) pts3;
	];
	new Num_by_num.bubble_dataset ~name:"ds4"
	  ~color:(color ~r:1. ~g:0. ~b:0. ~a:0.5)
	  [| triple 0.5 0.5 0.5;
	     triple 1.0 1.0 1.0;
	     triple 2.0 2.0 2.0;
	     triple 4.0 3.5 4.0;
	     triple 8.0 5.5 8.0;
	  |];
	new Num_by_num.vertical_errbar_dataset
	  [| triple 0.5 0.5 (0.5 /. 2.);
	     triple 1.0 1.0 (1.0 /. 2.);
	     triple 2.0 2.0 (2.0 /. 2.);
	     triple 4.0 3.5 (4.0 /. 2.);
	     triple 8.0 5.5 (8.0 /. 2.);
	  |];
	new Num_by_num.horizontal_errbar_dataset
	  [| triple 0.5 0.5 (0.5 /. 3.);
	     triple 1.0 1.0 (1.0 /. 3.);
	     triple 2.0 2.0 (2.0 /. 3.);
	     triple 4.0 3.5 (4.0 /. 3.);
	     triple 8.0 5.5 (8.0 /. 3.);
	  |];
      ]


let main () =

  let size = 800 in
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

(*
    let plot = nominal_plot () in
*)
    let plot = numeric_plot () in
      plot#draw ctx;

      Cairo_png.surface_write_to_file surface "test.png"

let _ = main ()
