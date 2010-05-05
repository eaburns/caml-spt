(** The main function for the spt program.

    For now this is just for testing spt, but eventually it
    should be a full program that reads plots from a file (or stdin)
    and will build the plot to the output.

    @author eaburns
    @since 2010-04-15
*)

open Geometry
open Drawing
open Spt
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
  let next_dash = Num_by_num.default_dash_factory () in
  let next_glyph = Num_by_num.default_glyph_factory () in
  let next_line_err = Num_by_num.line_errbar_factory next_dash () in
    ignore (next_line_err ());
    new Num_by_num.plot
      ~title:"Title text"
      ~xlabel:"X label text"
      ~ylabel:"Y label text"
      [
	Num_by_num.scatter_errbar_dataset
	  ~name:"Scatter with error bars"
	  ~color:green (next_glyph ())
	  [|
	    [| point 0.5 7.5; point 1.0 1.4;
	       point 2.0 7.0; point 10.0 3.5; |], Some "one";
	    [| point 8.5 10.5; point 1.8 8.1;
	       point 5.7 7.6; point 20.7 9.5; |], Some "two";
	    [| point 3.2 2.6; point 4.7 2.1;
	       point 3.6 7.7; point 10.0 3.7; |], Some "three";
	    [| point 1.9 9.5; point 9.2 8.0;
	       point 8.5 7.1; point 10.1 3.9; |], Some "four";
	  |];

	Num_by_num.lines_points_dataset (next_dash ()) (next_glyph ())
	  ~name:"Lines and points"
	  [| point 0.5 7.5; point 1.0 1.4; point 2.0 7.0; point 10.0 3.5; |];

	new Num_by_num.line_errbar_dataset
	  ~name:"Lines with error bars"
	  ~color:blue (next_line_err ())
	  [|
	    [| point 0.5 17.5; point 1.0 11.4;
	       point 2.0 17.0; point 10.0 13.5; |];
	    [| point 8.5 17.5; point 1.8 17.1;
	       point 5.7 12.6; point 20.7 13.5; |];
	    [| point 3.2 17.6; point 4.7 12.1;
	       point 3.6 17.7; point 10.0 13.7; |];
	    [| point 1.9 17.5; point 9.2 18.0;
	       point 8.5 17.1; point 10.1 13.9; |];
	  |];
      ]



let main () =
  let plot = num_by_nom_plot () in
    plot#display

let _ = main ()
