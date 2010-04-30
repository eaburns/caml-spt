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
open GMain

let num_by_nom_plot () =
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


let num_by_num_plot () =
  let pts0 =
    [| point 0.5 6.0; point 1.3 2.0; point 7.3 8.1; point 3.8 0.05; |]
  and pts1 =
    [| point 10.5 6.0; point 3.3 2.0; point 2.3 8.1; point 9.8 0.05; |]
  in
  let next_dash = Num_by_num.default_dash_factory () in
  let next_glyph = Num_by_num.default_glyph_factory () in
  let next_line_err = Num_by_num.line_errbar_factory next_dash () in
    new Num_by_num.plot
      ~title:"Title text"
      ~xlabel:"X label text"
      ~ylabel:"Y label text"
      [
	new Num_by_num.line_errbar_dataset
	  ~name:"ds0" ~color:red (next_line_err ())
	  [|
	    [| point 0.5 0.5; point 1.0 1.0; point 2.0 2.0; point 10.0 3.5; |];
	    [| point 8.5 4.5; point 1.8 7.7; point 5.7 2.6; point 20.7 7.5; |];
	    [| point 3.2 1.6; point 4.7 2.1; point 3.6 7.7; point 10.0 0.7; |];
	    [| point 1.9 7.5; point 9.2 7.0; point 8.5 7.1; point 10.1 3.9; |];
	  |];
	new Num_by_num.line_errbar_dataset
	  ~name:"ds1" ~color:green (next_line_err ())
	  [|
	    [| point 0.5 7.5; point 1.0 1.4; point 2.0 7.0; point 10.0 3.5; |];
	    [| point 8.5 4.5; point 1.8 7.1; point 5.7 2.6; point 20.7 3.5; |];
	    [| point 3.2 2.6; point 4.7 2.1; point 3.6 7.7; point 10.0 3.7; |];
	    [| point 1.9 9.5; point 9.2 8.0; point 8.5 7.1; point 10.1 3.9; |];
	  |];
	new Num_by_num.line_errbar_dataset
	  ~name:"ds2" ~color:blue (next_line_err ())
	  [|
	    [| point 0.5 7.5; point 1.0 1.4; point 2.0 7.0; point 10.0 3.5; |];
	    [| point 8.5 7.5; point 1.8 7.1; point 5.7 2.6; point 20.7 3.5; |];
	    [| point 3.2 7.6; point 4.7 2.1; point 3.6 7.7; point 10.0 3.7; |];
	    [| point 1.9 7.5; point 9.2 8.0; point 8.5 7.1; point 10.1 3.9; |];
	  |];
	new Num_by_num.line_errbar_dataset
	  ~name:"ds3" (next_line_err ())
	  [|
	    [| point 0.5 7.5; point 1.0 1.4; point 2.0 7.0; point 10.0 3.5; |];
	    [| point 8.5 7.5; point 1.8 7.1; point 5.7 2.6; point 20.7 3.5; |];
	    [| point 3.2 7.6; point 4.7 2.1; point 3.6 7.7; point 10.0 3.7; |];
	    [| point 1.9 7.5; point 9.2 8.0; point 8.5 7.1; point 10.1 3.9; |];
	  |];
	new Num_by_num.composite_dataset ~name:"ds0" [
	  new Num_by_num.line_dataset ~name:"ds0" (next_dash ()) pts0;
	  new Num_by_num.scatter_dataset ~name:"ds0" (next_glyph ()) pts0;
	  new Num_by_num.label_dataset ~name:"ds0"
	    (Array.mapi
	       (fun i pt -> { pt with y = pt.y +. 1. },
		  Printf.sprintf "lable-%d" i)
	       pts0);
	];
	new Num_by_num.composite_dataset ~name:"ds1" [
	  new Num_by_num.line_dataset ~name:"ds1" (next_dash ()) pts1;
	  new Num_by_num.scatter_dataset ~name:"ds1" (next_glyph ()) pts1;
	];
      ]



let main () =
  let plot = num_by_num_plot () in
    plot#display
    (*Ml_plot_cairo.as_png plot "/var/tmp/test.png";
    Ml_plot_cairo.as_ps plot "/var/tmp/test.ps";
    Ml_plot_cairo.as_pdf plot "/var/tmp/test.pdf"*)

let _ = main ()
