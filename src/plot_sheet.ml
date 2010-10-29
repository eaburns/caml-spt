(** A sheet of plots.

    @author eaburns
    @since 2010-09-08
*)

open Drawing
open Geometry
open Verbosity

let plot_size ctx theta plot =
  (** [plot_size ctx theta plot] gets the width and height of the plot
      when rotated at the given angle. *)
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Geometry.rotated_size ~theta ~w ~h


let draw_plot ctx plot ~x ~y ~theta =
  (** [draw_plot ctx plot ~x ~y ~theta] draws the given plot centered
      at [x],[y].  [theta] is in radians. *)
  let units = ctx.units in
  let w = units plot#width and h = units plot#height in
    Drawing.save_transforms ctx;
    Drawing.translate ctx x y;
    Drawing.rotate ctx theta;
    Drawing.translate ctx ~-.(w /. 2.) ~-.(h /. 2.);
    plot#draw ctx;
    Drawing.restore_transforms ctx


class plot_sheet lplots =
  (** [plat_sheet lplots] is a simple class that draws plots at a given
      x,y and theta.  [lplots] is a list of [located_plot]s. *)
object(self)

  inherit Spt.plot None

  val angle = ref Geometry.pi

  method draw ctx =
    let units = ctx.units in
      List.iter (fun (xlen, ylen, theta, p) ->
		   let x = units xlen and y = units ylen in
		     draw_plot ctx p ~x ~y ~theta)
	lplots

end

(** {1 Single page plots} ****************************************)

let centered_on ~w ~h ~pad plot =
  (** [centered_on ~w ~h ~pad plot] centers [plot] on a sheet of size
      [w]x[h] with [pad] between the edges of the plot and the ends of the
      sheet. *)
  let plot = Oo.copy plot in
  let wpt = Length.as_pt w and hpt = Length.as_pt h in
  let ppt = Length.as_pt pad in
  let sheet =
    new plot_sheet [ (Length.Pt (wpt /. 2.), Length.Pt (hpt /. 2.), 0., plot) ]
  in
    sheet#set_size ~w:(Length.Pt wpt) ~h:(Length.Pt hpt);
    plot#set_size ~w:(Length.Pt (wpt -. 2. *. ppt))
      ~h:(Length.Pt (hpt -. 2. *. ppt));
    sheet


let us_letter ?(landscape=false) plot =
  (** [us_letter ?landscape plot] clone the given plot to a us letter
      sized sheet. *)
  let win = if landscape then 11. else 8.5 in
  let hin = if landscape then 8.5 else 11. in
    centered_on ~w:(Length.In win) ~h:(Length.In hin)
      ~pad:(Length.In 0.25) plot


let double_letter ?(landscape=false) plot =
  (** [double_letter ?landscape plot] clone the given plot to a double
      us letter sized sheet (11x17). *)
  let win = if landscape then 17. else 11. in
  let hin = if landscape then 11. else 17. in
    centered_on ~w:(Length.In win) ~h:(Length.In hin)
      ~pad:(Length.In 0.25) plot


(** {1 Montages} ****************************************)


let rec take_n n ?(accum=[]) ps =
  (** [take_n n ?accum ps] take the first [n] elements from [ps]. *)
  if (List.length accum) = n then
    List.rev accum, ps
  else
    match ps with
      | hd :: tl -> take_n n ~accum:(hd :: accum) tl
      | _ -> List.rev accum, []


let rec group n ?(accum=[]) ps =
  (** [group n ?accum ps] gets groups of size [n]. *)
  let line, rest = take_n n ps in
    if line = [] then List.rev accum else group n ~accum:(line :: accum) rest


let entry_size_pt ~pad_pt ~w ~h ~nrows ~ncols =
  (** [entry_size_pt ~pad_pt ~w ~h ~nrows ~ncols] computes the size of
      each entry. *)
  let ncolsf = float ncols and nrowsf = float nrows in
  let total_col_pad = pad_pt *. (ncolsf +. 1.) in
  let total_row_pad = pad_pt *. (nrowsf +. 1.) in
  let wpt = (Length.as_pt w) -. total_col_pad in
  let hpt = (Length.as_pt h) -. total_row_pad in
    wpt /. ncolsf, hpt /. nrowsf



let layout_page ~w ~h ~went ~hent ~pad ~ncols plots =
  (** [layout_page ~w ~h ~went ~hent ~pad ~ncols plots] lays out the
      plots on a single page.  The result is a new plot sheet for the
      given page. *)
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
		  let lp = Length.Pt x, Length.Pt y, 0., p in
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


let montage ~w ~h ~pad ?nrows ?ncols plots =
  (** [montage ~w ~h ~pad ?nrows ?ncols plots] montage the given set of
      plots across a bunch of pages.  The resulting pages have size
      [w]x[h]. *)
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


let us_letter_montage ?(landscape=false) ?nrows ?ncols plots =
  (** [us_letter_montage ?landscape ?nrows ?ncols plots] montages
      [plots] across us letter sized pages. *)
  let win = if landscape then 11. else 8.5 in
  let hin = if landscape then 8.5 else 11. in
    montage ~w:(Length.In win) ~h:(Length.In hin) ~pad:(Length.In 0.25)
      ?nrows ?ncols plots


let double_letter_montage ?(landscape=false) ?nrows ?ncols plots =
  (** [double_letter_montage ?landscape ?nrows ?ncols plots] montages
      [plots] across 11x17in pages. *)
  let win = if landscape then 17. else 11. in
  let hin = if landscape then 11. else 17. in
    montage ~w:(Length.In win) ~h:(Length.In hin) ~pad:(Length.In 0.25)
      ?nrows ?ncols plots
