(** A sheet of plots.

    TODO: Change located plots to use a record instead of a 4-tuple

    @author eaburns
    @since 2010-09-08
*)

open Printf
open Drawing
open Geometry
open Verbosity

(** The type of a plot that can be put on a plot sheet. *)
class type sheetable_plot = object
  method draw : Drawing.context -> unit
  method set_size : w:Length.t -> h:Length.t -> unit
  method height : Length.t
  method width : Length.t
end

(** A located plot. *)
type lplot = {
  xlen : Length.t;
  ylen : Length.t;
  theta : float;
  plot : sheetable_plot;
}

(** [plot_size ctx theta plot] gets the width and height of the plot
    when rotated at the given angle. *)
let plot_size ctx theta plot =
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Geometry.rotated_size ~theta ~w ~h


(** [draw_plot ctx plot ~x ~y ~theta] draws the given plot centered
    at [x],[y].  [theta] is in radians. *)
let draw_plot ctx plot ~x ~y ~theta =
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Drawing.save_transforms ctx;
    Drawing.translate ctx x y;
    Drawing.rotate ctx theta;
    Drawing.translate ctx ~-.(w /. 2.) ~-.(h /. 2.);
    plot#draw ctx;
    Drawing.restore_transforms ctx


(** [plat_sheet lplots] is a simple class that draws plots at a
    given x,y and theta.  [lplots] is a list of [located_plot]s. *)
class plot_sheet lplots =
object(self)

  inherit Spt.plot None

  val angle = ref Geometry.pi

  method draw ctx =
    let units = ctx.units in
      List.iter
	(fun { xlen = xlen; ylen = ylen; theta = theta; plot = plot } ->
	   let x = units xlen and y = units ylen in
	     draw_plot ctx plot ~x ~y ~theta)
	lplots

end

(** {1 Single page plots} *)

(** [centered_on ~w ~h ~pad plot] centers [plot] on a sheet of size
    [w]x[h] with [pad] between the edges of the plot and the ends of
    the sheet. *)
let centered_on ~w ~h ~pad plot =
  let plot = Oo.copy plot in
  let wpt = Length.as_pt w and hpt = Length.as_pt h in
  let ppt = Length.as_pt pad in
  let sheet =
    new plot_sheet [ { xlen = Length.Pt (wpt /. 2.);
		       ylen = Length.Pt (hpt /. 2.);
		       theta = 0.;
		       plot = plot } ]
  in
    sheet#set_size ~w:(Length.Pt wpt) ~h:(Length.Pt hpt);
    plot#set_size ~w:(Length.Pt (wpt -. 2. *. ppt))
      ~h:(Length.Pt (hpt -. 2. *. ppt));
    sheet


(** [us_letter ?landscape plot] clone the given plot to a us letter
    sized sheet. *)
let us_letter ?(landscape=false) plot =
  let win = if landscape then 11. else 8.5 in
  let hin = if landscape then 8.5 else 11. in
    centered_on ~w:(Length.In win) ~h:(Length.In hin)
      ~pad:(Length.In 0.25) plot


(** [double_letter ?landscape plot] clone the given plot to a double
    us letter sized sheet (11x17). *)
let double_letter ?(landscape=false) plot =
  let win = if landscape then 17. else 11. in
  let hin = if landscape then 11. else 17. in
    centered_on ~w:(Length.In win) ~h:(Length.In hin)
      ~pad:(Length.In 0.25) plot


(************************************************************)
(** {1 Montages} *)


(** [take_n n ?accum ps] take the first [n] elements from [ps]. *)
let rec take_n n ?(accum=[]) ps =
  if (List.length accum) = n then
    List.rev accum, ps
  else
    match ps with
      | hd :: tl -> take_n n ~accum:(hd :: accum) tl
      | _ -> List.rev accum, []


(** [group n ?accum ps] gets groups of size [n]. *)
let rec group n ?(accum=[]) ps =
  let line, rest = take_n n ps in
    if line = [] then List.rev accum else group n ~accum:(line :: accum) rest


(** [entry_size_pt ~pad_pt ~w ~h ~nrows ~ncols] computes the size of
    each entry. *)
let entry_size_pt ~pad_pt ~w ~h ~nrows ~ncols =
  let ncolsf = float ncols and nrowsf = float nrows in
  let total_col_pad = pad_pt *. (ncolsf +. 1.) in
  let total_row_pad = pad_pt *. (nrowsf +. 1.) in
  let wpt = (Length.as_pt w) -. total_col_pad in
  let hpt = (Length.as_pt h) -. total_row_pad in
    wpt /. ncolsf, hpt /. nrowsf



(** [layout_page ~w ~h ~went ~hent ~pad ~ncols plots] lays out the
    plots on a single page.  The result is a new plot sheet for the
    given page. *)
let layout_page ~w ~h ~went ~hent ~pad ~ncols plots =
  let pad_pt = Length.as_pt pad in
  let went_pt = Length.as_pt went in
  let hent_pt = Length.as_pt hent in
  let dx_pt = went_pt +. pad_pt and dy_pt = hent_pt +. pad_pt in
  let xoff_pt = (went_pt /. 2.) +. pad_pt in
  let yoff_pt = (hent_pt /. 2.) +. pad_pt in
  let r = ref 0 and c = ref 0 in
    vprintf verb_debug "page --------------------\n";
    let lplots =
      List.map (fun p ->
		  let x = (float !c) *. dx_pt +. xoff_pt in
		  let y = (float !r) *. dy_pt +. yoff_pt in
		  let p = Oo.copy p in
		  let lp = { xlen = Length.Pt x;
			     ylen = Length.Pt y;
			     theta = 0.;
			     plot = p }
		  in
		    vprintf verb_debug "c=%d, r=%d, x=%f, y=%f\n" !c !r x y;
		    p#set_size ~w:went ~h:hent;
		    if !c = ncols - 1 then r := (!r + 1);
		    c := (!c + 1) mod ncols;
		    lp)
	plots
    in
    let page = new plot_sheet lplots in
      page#set_size ~w ~h;
      page


(** [montage ~w ~h ~pad ?nrows ?ncols plots] montage the given set
    of plots across a bunch of pages.  The resulting pages have size
    [w]x[h]. *)
let montage ~w ~h ~pad ?nrows ?ncols plots =
  let nplots = List.length plots in
  let square_off n = truncate (ceil (sqrt (float n))) in
  let ncols = match ncols with Some c -> c | _ -> square_off nplots in
  let nrows = match nrows with Some r -> r | _ -> square_off nplots in
  let pad_pt = Length.as_pt pad in
  let went_pt, hent_pt = entry_size_pt ~pad_pt ~w ~h ~nrows ~ncols in
  let went = Length.Pt went_pt and hent = Length.Pt hent_pt in
  let ents_per_page = ncols * nrows in
  let pages = group ents_per_page plots in
    vprintf verb_debug "entry width: %fpt\n" went_pt;
    vprintf verb_debug "entry height: %fpt\n" hent_pt;
    vprintf verb_debug "entries per page: %d\n" ents_per_page;
    vprintf verb_optional "number of pages: %d\n" (List.length pages);
    List.map (layout_page ~w ~h ~went ~hent ~pad ~ncols) pages


(** [us_letter_montage ?landscape ?nrows ?ncols plots] montages
    [plots] across us letter sized pages. *)
let us_letter_montage ?(landscape=false) ?nrows ?ncols plots =
  let win = if landscape then 11. else 8.5 in
  let hin = if landscape then 8.5 else 11. in
    montage ~w:(Length.In win) ~h:(Length.In hin) ~pad:(Length.In 0.25)
      ?nrows ?ncols plots


(** [double_letter_montage ?landscape ?nrows ?ncols plots] montages
    [plots] across 11x17in pages. *)
let double_letter_montage ?(landscape=false) ?nrows ?ncols plots =
  let win = if landscape then 17. else 11. in
  let hin = if landscape then 11. else 17. in
    montage ~w:(Length.In win) ~h:(Length.In hin) ~pad:(Length.In 0.25)
      ?nrows ?ncols plots


(************************************************************)
(** {1 Scatter-plot matrix } *)

(** Creates the scatter plot to use in the entry of the matrix. *)
let scatter_plot_entry label_text_style tick_text_style glyph ?color
    ?point_radius ?xlabel ?ylabel xs ys =
  assert ((Array.length xs) = (Array.length ys));
  let npts = Array.length xs in
  let pts = Array.init npts (fun i -> point xs.(i) ys.(i)) in
  let scatter = Num_by_num.scatter_dataset glyph ?color ?point_radius pts in
    Num_by_num.plot ?label_text_style ?tick_text_style ?xlabel ?ylabel
      [scatter]


(** Creates an entry of the matrix. *)
let matrix_entry label_text_style tick_text_style glyph ?color
    ?point_radius ~n ~r ~c ~xoff ~yoff
    ~ent_w ~ent_h data =
  let xname, xs = data.(c) and yname, ys = data.(r) in
  let xlabel = if r = n - 1 then Some xname else None in
  let ylabel = if c = 0 then Some yname else None in
    if (Array.length xs) <> (Array.length ys) then
      invalid_arg (sprintf "Differing number of points for %s and %s"
		     xname yname);
    let plot =
      scatter_plot_entry label_text_style tick_text_style glyph ?color
	?point_radius ?xlabel ?ylabel xs ys
    in
    let xlen = Length.Pt (float c *. ent_w +. xoff) in
    let ylen = Length.Pt (float r *. ent_h +. yoff) in
    let lplot = { xlen = xlen; ylen = ylen; theta = 0.;
		  plot = (plot :> sheetable_plot); }
    in
      plot#set_size ~w:(Length.Pt ent_w) ~h:(Length.Pt ent_h);
      lplot


(** Creates a scatter-plot matrix. *)
let scatter_plot_matrix ?(glyph=Drawing.Ring_glyph) ?color ?point_radius
    ?label_text_style ?tick_text_style ~w ~h data =
  let pad = Length.as_pt (Length.In 0.5) in
  let n = Array.length data in
  let nf = float n in
  let ent_w = ((Length.as_pt w) -. 2. *. pad) /. nf in
  let ent_h = ((Length.as_pt h) -. 2. *. pad) /. nf in
  let xoff = ent_w /. 2. +. pad and yoff = ent_h /. 2. +. pad in
  let lplots = ref [] in
    for r = 0 to n - 1 do
      for c = 0 to n - 1 do
	let lplot =
	  matrix_entry label_text_style tick_text_style glyph ?color
	    ?point_radius ~n ~r ~c ~xoff ~yoff ~ent_w ~ent_h data
	in
	  lplots := lplot :: !lplots
      done
    done;
    let page = new plot_sheet !lplots in
      page#set_size ~w ~h;
      page
