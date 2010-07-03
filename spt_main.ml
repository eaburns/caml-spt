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

let num_by_nom_plot () =
  (*
    let barchart =
    Barchart_dataset.barchart_datasets
    ~use_color:true
    ["jan", 10.; "feb", 40.; "mar", 20.; "apr", -10.;
    "may", 10.; "jun", 10.; "jul", 10.; "aug", 10.;
    "sep", 10.; "oct", 10.; "nov", 10.; "dec", 10.;
    ]
    in
  *)
  (*let bar_err =
    (Barchart_dataset.barchart_errbar_datasets
    ~group:"Winter"
    ~use_color:true
    [
    "dec", (Array.init 1 (fun _ -> Random.float 1000.));
    "jan", (Array.init 1 (fun _ -> Random.float 1000.));
    "feb", (Array.init 1 (fun _ -> Random.float 1000.));
    ]) @
    (Barchart_dataset.barchart_errbar_datasets
    ~group:"Spring"
    ~use_color:true
    [
    "mar", (Array.init 10 (fun _ -> Random.float 1000.));
    "apr", (Array.init 10 (fun _ -> Random.float 1000.));
    "may", (Array.init 10 (fun _ -> Random.float 1000.));
    ]) @
    (Barchart_dataset.barchart_errbar_datasets
    ~group:"Summer"
    ~use_color:true
    [
    "jun", (Array.init 100 (fun _ -> Random.float 1000.));
    "jul", (Array.init 100 (fun _ -> Random.float 1000.));
    "aug", (Array.init 100 (fun _ -> Random.float 1000.));
    ]) @
    (Barchart_dataset.barchart_errbar_datasets
    ~group:"Autumn"
    ~use_color:true
    [
    "sep", (Array.init 1000 (fun _ -> Random.float 1000.));
    "oct", (Array.init 1000 (fun _ -> Random.float 1000.));
    "nov", (Array.init 1000 (fun _ -> Random.float 1000.));
    ])*)
  let stacked =
    Barchart_dataset.stacked_barchart_datasets
      ~group:"Stacked"
      ~fill_factory:(Factories.default_color_fill_pattern_factory ())
      [
	(Some "Stack A"), [|"a1", 10.; "a2", 2.; "a3", 9.;|];
	(Some "Stack B"), [|"b1", 8.; "b2", 13.; "b3", 1.;|];
	  None,[|"c1", 10.; "c2", 5.; "c3", 7.;|];
      ]

  and layered =
    Barchart_dataset.layered_barchart_datasets
      ~group:"Layered"
      ~fill_factory:(Factories.default_color_fill_pattern_factory ())
      [ (Some "Layer A"), [|"a1", 10.; "a2", 2.; "a3", 9.;|];
	(Some "Layer B"), [|"b1", 8.; "b2", 13.; "b3", 1.;|];
	None, [|"c1", 10.; "c2", 5.; "c3", 7.;|]]
  in
    Num_by_nom.plot ~title:"Title text" ~ylabel:"Y label text"
      (stacked @ layered)

let num_by_num_plot () =
  let next_dash = Factories.default_dash_factory () in
  let next_glyph = Factories.default_glyph_factory () in
    Num_by_num.plot
      ~title:"Title text"
      ~xlabel:"X label text"
      ~ylabel:"Y label text"
      ~sort_legend:false
      ~legend_loc:Legend.Lower_right
      [
	(*
	  Num_by_num.histogram_dataset
	  (next_dash ())
	  (Array.init 1000 (fun _ -> Random.float 10000000.))
	*)
	Num_by_num.bestfit_dataset
	  ~dashes:(next_dash ())
	  ~glyph:(next_glyph ())
	  ~name:"Zero"
	  [| point 0. 0.; point 1. 1.; point 2. 2.; point 3. 3.; |];
	Num_by_num.bestfit_dataset
	  ~dashes:(next_dash ())
	  ~glyph:(next_glyph ())
	  ~degree:2
	  ~name:"One"
	  [| point 0. 0.; point 1. 1.; point 2. 4.; point 3. 9.; |];
      ]

let rand_color () =
  let color = { Drawing.r = Random.float 1.;
		Drawing.g = Random.float 1.;
		Drawing.b = Random.float 1.;
		Drawing.a = 1. }
  in color

let rec rand_tree d max_depth max_br =
  let color = rand_color () in
    if d = max_depth
    then { Tree_vis.color = color;
	   Tree_vis.succs = [||] }
    else begin
      let br = if max_br <= 2 then 2 else (Random.int (max_br - 2)) + 2 in
      let succs =
	Array.init br (fun _ -> rand_tree (d + 1) max_depth max_br)
      in
	{ Tree_vis.color = color;
	  Tree_vis.succs = succs;
	}
    end


let tree_plot () =
  Tree_vis.plot Tree_vis.Sunburst.style (rand_tree 0 10 3)


let main () =
  Random.self_init ();
  Verbosity.Verb_level.set Verbosity.verb_debug;
  let plot = num_by_num_plot () in
    plot#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
(*
    plot#output "plot.png"
*)
    plot#display

let _ = main ()
