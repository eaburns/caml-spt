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

(*
    let plot =  (new num_by_num_plot
		   ~title:(Some "Title text")
		   ~xlabel:(Some "X label text")
		   ~ylabel:(Some "Y label text")
		   ~scale:(rectangle 0. 5. ~-.0.1 0.9)
		   [])
    in
*)
    let num_by_nom_datasets = [
      object
	inherit num_by_nom_dataset "Dataset one"
	method y_min_and_max = 0., 0.
	method draw _ _ ~y_min:_ ~y_max:_ ~width:_ _ = ()
      end;
      object
	inherit num_by_nom_dataset "Dataset two"
	method y_min_and_max = 0., 0.
	method draw _ _ ~y_min:_ ~y_max:_ ~width:_ _ = ()
      end;
      object
	inherit num_by_nom_dataset
	  "Some third dataset that has a very long name"
	method y_min_and_max = 0., 0.
	method draw _ _ ~y_min:_ ~y_max:_ ~width:_ _ = ()
      end;
    ] in
    let plot =  (new num_by_nom_plot
		   ~title:(Some "Title text")
		   ~ylabel:(Some "Y label text")
		   ~y_min:0.
		   ~y_max:1.
		   num_by_nom_datasets)
    in
      plot#draw ctx;

      Cairo_png.surface_write_to_file surface "test.png"

let _ = main ()
