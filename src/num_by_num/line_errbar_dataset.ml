(** Lines with error bars.

    @author eaburns
    @since 2010-04-30
*)

open Verbosity
open Num_by_num_dataset
open Geometry

type line_errbar_style = {
  dashes : Length.t array;
  (* The dash pattern for the line. *)
  number : int;
  (* The number of the current line_errbar_dataset. *)
  count : int ref;
  (* A reference that counts the total number of associated
     line_errbar_datasets. *)
}


let line_errbar_factory next_dashes () =
  (** [line_errbar_factory next_dashes ()] gets a line and errorbar
      factory.  This helps choosing where the error bars reside. *)
  let count = ref 0 in
    (fun () ->
       incr count;
       {
	 dashes = next_dashes ();
	 number = !count - 1;
	 count = count;
       })


let line_domain l =
  (** [line_domain l] gets the domain of the line. *)
  Array.fold_left (fun r pt ->
		     let min = if pt.x < r.min then pt.x else r.min
		     and max = if pt.x > r.max then pt.x else r.max
		     in range min max)
    (range infinity neg_infinity) l


let common_domain lines =
  (** [common_domain lines] get the domain that the given lines have
      in common. *)
  let domains = Array.map line_domain lines in
    Array.fold_left (fun cur d ->
		       let min = if d.min < cur.min then cur.min else d.min
		       and max = if d.max > cur.max then cur.max else d.max
		       in range min max)
      (range neg_infinity infinity) domains


let check_lines lines =
  (** [check_lines lines] throws out a nasty warning if the lines are
      not sorted on the x-value. *)
  Array.iter
    (fun line ->
       ignore (Array.fold_left
		 (fun min pt ->
		    if pt.x <= min
		    then
		      vprintf verb_normal
			"Lines are not sorted on x-value %f <= %f \n" pt.x min;
		    pt.x)
		 neg_infinity line))
    lines


let mean_line ?(xs=[||]) domain lines =
  (** [mean_line ?xs domain lines] get a line that is the mean of all
      of the given lines.  [xs] is an array of x-values to ensure are
      on the line.  Each value in [xs] must already be in [domain]. *)
  let module Float_set = Set.Make(struct
				    type t = float
				    let compare (a:float) b = compare a b
				  end) in
    (*check_lines lines;*)
  let min = domain.min and max = domain.max in
  let init_xset =
      Array.fold_left (fun s x -> assert (x >= min); assert (x <= max);
			  Float_set.add x s)
	 Float_set.empty xs in
  let xs =
    Array.fold_left (fun set l ->
		       Array.fold_left (fun set pt ->
					  let x = pt.x in
					    if x >= min && x <= max
					    then Float_set.add x set
					    else set)
			 set l)
      init_xset lines
  in
    Array.of_list (Float_set.fold
		     (fun x lst ->
			let ys = Array.map (fun l -> interpolate l x) lines in
			let mean = Statistics.mean ys in
			  (point x mean) :: lst)
		     xs [])


let errbars ~xrange ~num ~count ~domain lines =
  (** [errbars ~xrange ~num ~count ~domain lines] get the error
      bars. *)
  let min = domain.min and max = domain.max in
  let x = ref min in
  let ngroups = 4. in
  let numf = float num and countf = float count in
  let group_width = (max -. min) /. ngroups in
  let delta = group_width /. countf in
  let group_start = delta /. 2. in
  let group_offs = (group_start +. (delta *. numf)) /. group_width in
  let group = ref (floor (min /. group_width)) in
  let intervals = ref [] in
    while !x < max do
      let x' = (!group *. group_width) +. (group_width *. group_offs) in
	if x' >= min && x' <= max
	then begin
	  let ys = Array.map (fun l -> interpolate l x') lines in
	  let mean, ci = Statistics.mean_and_interval ys in
	    intervals := (triple x' mean ci) :: !intervals;
	end;
	x := x';
	group := !group +. 1.;
    done;
    Array.of_list !intervals


let mean_line_and_errbars ~num ~count lines =
  (** [mean_line_and_errbars ~num ~count lines] gets the mean line and
      the error bars. *)
  let domain = common_domain lines in
  let errbars = errbars ~xrange:domain ~num ~count ~domain lines in
  let mean = mean_line domain lines in
    mean, errbars


type style_cache_entry =
    {
      n : int;
      t : int;
      comp : composite_dataset option;
    }


let cache_key ~n ~t = { n = n; t = t; comp = None }
  (** [cache_key ~n ~t] builds a key for the cache. *)

module Style_cache = Weak.Make(struct
				 type t = style_cache_entry
				 let equal a b = a.n = b.n && a.t = b.t
				 let hash a = Hashtbl.hash (a.n, a.t)
			       end)



let filter_lines lines =
  (** [filter_lines lines] filters out bad lines.  For example, lines
      with only a single point. *)
  let n = Array.length lines in
  let nrem =
    Array.fold_left
      (fun s l -> if (Array.length l) < 2 then s + 1 else s)
      0 lines
  in
  let i = ref 0 in
    if !nrem > 0 then
      vprintf verb_normal
	"Ignoring %d lines with fewer than two points, %d lines remaining\n"
	nrem (n - nrem);
    Array.init (n - nrem)
      (fun _ ->
	 while (Array.length lines.(!i)) < 2 do incr i done;
	 assert (!i < n);
	 incr i;
	 assert (!i > 0);
	 lines.(!i - 1))


class line_errbar_dataset style ?color ?line_width ?name lines =
  (** [line_errbar_dataset style ?color ?line_width ?name lines] makes a
      line and error bar dataset. *)
  let lines = filter_lines lines in
object (self)
  inherit dataset ?name ()

  val style_cache = Style_cache.create 10
    (** Caches the composite dataset based on the style. *)


  method private build_composite =
    (** [build_composite] builds the composite dataset. *)
    let points, bars =
      mean_line_and_errbars style.number !(style.count) lines
    in
      new composite_dataset ?name
	[(new Line_dataset.line_dataset style.dashes
	    ?color ?line_width ?name points);
	 (new Errbar_dataset.vertical_errbar_dataset ?color bars)]


  method private composite =
    (** [composite] either builds the composite or returns it from the
	cache. *)
    let key = cache_key style.number !(style.count) in
    let entry =
      try Style_cache.find style_cache key
      with Not_found ->
	let comp = self#build_composite in
	let ent =
	  { n = style.number; t = !(style.count); comp = Some comp; }
	in
	  Style_cache.add style_cache ent;
	  ent
    in match entry.comp with
      | None ->
	  let comp = self#build_composite in
	  let ent =
	    { n = style.number; t = !(style.count); comp = Some comp; }
	  in
	    Style_cache.add style_cache ent;
	    comp
      | Some comp -> comp


  method dimensions = self#composite#dimensions

  method mean_y_value src = self#composite#mean_y_value src

  method residual ctx ~src ~dst = self#composite#residual ctx ~src ~dst

  method draw ctx ~src ~dst = self#composite#draw ctx ~src ~dst

  method draw_legend ctx ~x ~y = self#composite#draw_legend ctx ~x ~y

  method legend_dimensions ctx = self#composite#legend_dimensions ctx

  method avg_slope = self#composite#avg_slope

end


let line_errbar_dataset dashes ?line_width ?color ?name lines =
  new line_errbar_dataset dashes ?line_width ?color ?name lines


let line_errbar_datasets ?(color=false) name_by_lines_list =
  let next_dash = Factories.default_dash_factory () in
  let next_style = line_errbar_factory next_dash () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> Drawing.black))
  in
    List.map (fun (name, lines) ->
		line_errbar_dataset (next_style ())
		  ~color:(next_color()) ?name lines)
      name_by_lines_list


let default_radius =
  Length.Pt ((Length.as_pt Scatter_dataset.default_radius) /. 2.)


let scatter_errbar_lines_dataset
    glyph dash ?color ?(point_radius=default_radius) ?line_width ?name sets =
  let pts, lbls, x_errs, y_errs =
    Array.fold_left (fun (pts, lbls, x_errs, y_errs) (vls, name) ->
		       let xs = Array.map (fun p -> p.x) vls
		       and ys = Array.map (fun p -> p.y) vls in
		       let mu_x, int_x = Statistics.mean_and_interval xs
		       and mu_y, int_y = Statistics.mean_and_interval ys in
		       let pt = point mu_x mu_y in
			 Printf.eprintf "%f x %f\n" mu_x mu_y;
		       let lbls' = match name with
			 | Some txt -> (pt, txt) :: lbls
			 | None -> lbls
		       in
			 (pt :: pts,
			  lbls',
			  triple mu_x mu_y int_x :: x_errs,
			  triple mu_x mu_y int_y :: y_errs))
      ([], [], [], []) sets in
  let scatter =
    Scatter_dataset.scatter_dataset glyph ?color
      ~point_radius (Array.of_list pts)
  and labels =
    new Label_dataset.label_dataset
      ~yoff:(Length.Pt ~-.(Length.as_pt point_radius)) ~xoff:point_radius
      ~xloc:Label_dataset.Label_after
      ~yloc:Label_dataset.Label_above
      (Array.of_list lbls)
  and horiz_err =
    new Errbar_dataset.horizontal_errbar_dataset ?color (Array.of_list x_errs)
  and vert_err =
    new Errbar_dataset.vertical_errbar_dataset ?color (Array.of_list y_errs)
  and line =
    Line_dataset.line_dataset dash ?color ?name ?line_width (Array.of_list pts)
  in
    new composite_dataset ?name [scatter; line; horiz_err; vert_err; labels;]



let scatter_errbar_lines_datasets ?(color=false) name_by_sets_list =
  let next_glyph = Factories.default_glyph_factory ()
  and next_dash = Factories.default_dash_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> Drawing.black))
  in
    List.map (fun (name,sets) ->
		scatter_errbar_lines_dataset (next_glyph())
		  (next_dash()) ~color:(next_color()) ~name sets)
      name_by_sets_list

(* EOF *)
