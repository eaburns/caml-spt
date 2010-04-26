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
open Num_by_num

let nominal_plot () =
  new num_by_nom_plot
    ~title:"Title text"
    ~ylabel:"Y label text"
    ~y_min:0.
    ~y_max:1.
    [
      (object
	 inherit num_by_nom_dataset "Dataset one"
	 method y_min_and_max = 0., 0.
	 method draw _ _ ~y_min:_ ~y_max:_ ~width:_ _ = ()
       end);
      (object
	 inherit num_by_nom_dataset "This is example dataset two"
	 method y_min_and_max = 0., 0.
	 method draw _ _ ~y_min:_ ~y_max:_ ~width:_ _ = ()
       end);
      (object
	 inherit num_by_nom_dataset
	   "Some third dataset that has a very long name"
	 method y_min_and_max = 0., 0.
	 method draw _ _ ~y_min:_ ~y_max:_ ~width:_ _ = ()
       end);
    ]


let numeric_plot () =
  new num_by_num_plot
    ~title:"Title text"
    ~xlabel:"X label text"
    ~ylabel:"Y label text"
    [
      new line_points_dataset ~name:"ds0" [ point 0.5 6.0;
					    point 1.3 2.0;
					    point 7.3 8.1;
					    point 3.8 0.05;
					  ];
      new scatter_dataset ~name:"ds1" [ point 8.0 1.117;
					point 3.1415926535 1.7;
					point 5.0 8.12;
				      ];
      new bubble_dataset ~name:"ds2" ~color:red [ (point 8.75 4.1), 5.;
						  (point 5.7 8.1), 2.;
						  (point 9.1 1.2), 1.;
						];
    ]


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

    let plot = numeric_plot () in
(*
    let plot = nominal_plot () in
*)
      plot#draw ctx;

      Cairo_png.surface_write_to_file surface "test.png"

let _ = main ()
