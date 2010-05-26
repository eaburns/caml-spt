(** Barcharts.

    @author jtd7 and eaburns
    @since 2010-05-26
*)

open Num_by_nom_dataset
open Drawing
open Geometry

(** {1 Simple barcharts} ****************************************)

class barchart_dataset fill_pattern ?(width=Length.Pt 1.) name value =
  (** A simple barchart.  This is one bar with a name and a height. *)
  let min_val = min 0. value
  and max_val = max 0. value in
object(self)

  inherit Num_by_nom_dataset.dataset name

  val style = { default_line_style with line_width = width; }


  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst in
    let y_min = tr (min 0. value)
    and y_max = tr (max 0. value) in
    let r = rectangle ~x_min:x ~x_max:(x +. width) ~y_min ~y_max in
      draw_fill_pattern ctx r fill_pattern;
      draw_line ctx ~style [point x y_min;
			    point x y_max;
			    point (x +. width) y_max;
			    point (x +. width) y_min;
			    point x y_min;]

end


let barchart_dataset fill_pattern ?(width=Length.Pt 1.) name data =
  (** [barchart_dataset fill_pattern ?width name data] makes a
      barchart dataset. *)
  new barchart_dataset fill_pattern ~width name data


let barchart_datasets
    ?(use_color=false)
    ?(width=Length.Pt 1.) ?(gname = "") values =
  (** [barchart_datasets ?use_color ?width ?gname values] makes a
      group of bars datasets. *)
  let next_fill =
    if use_color
    then Factories.default_color_fill_pattern_factory ()
    else Factories.default_fill_pattern_factory () in
  let bars =
    List.map
      (fun (nm,dt) -> new barchart_dataset (next_fill ()) ~width nm dt)
      values
  in new Num_by_nom.dataset_group gname bars


(** {1 Barcharts with error bars} ****************************************)


class barchart_errbar_dataset fill_pattern ?(width=Length.Pt 1.) name values =
  (** A barchart where the height of the bar is the mean of a set of
      values and error bars are computed to the 95\% confidence
      intervals. *)
  let mean, conf_interval = Statistics.mean_and_interval values in
  let min_val = min 0. (mean -. conf_interval)
  and max_val = max 0. (mean +. conf_interval) in
object(self)

  inherit Num_by_nom_dataset.dataset name

  val style = { default_line_style with line_width = width; }


  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst in
    let y_min = tr (min 0. mean)
    and y_max = tr (max 0. mean)
    and center = (width /. 2.) +. x in
    let r = rectangle ~x_min:x ~x_max:(x +. width) ~y_min ~y_max in
      draw_fill_pattern ctx r fill_pattern;
      draw_line ctx ~style [point x y_min;
			    point x y_max;
			    point (x +. width) y_max;
			    point (x +. width) y_min;
			    point x y_min;];
      Errbar.draw_up ctx ~style ~src ~dst
	~x:center ~y:mean ~mag:conf_interval;
      Errbar.draw_down ctx ~style ~src ~dst
	~x:center ~y:mean ~mag:conf_interval;

end

let barchart_errbar_dataset fill_pattern ?(width=Length.Pt 1.) name data =
  (** [barchart_errbar_dataset fill_pattern ?width name data] makes a
      barchart dataset where the bar is the mean value with error
      bars. *)
  new barchart_errbar_dataset fill_pattern ~width name data


let barchart_errbar_datasets
    ?(use_color=false) ?(width=Length.Pt 1.) ?(gname = "") values =
  (** [barchart_errbar_datasets ?use_color ?width ?gname values] makes
      a set of barcharts with error bars. *)
  let next_fill =
    if use_color
    then Factories.default_color_fill_pattern_factory ()
    else Factories.default_fill_pattern_factory () in
  let bars =
    List.map
      (fun (nm,dt) ->
	 new barchart_errbar_dataset (next_fill ()) ~width nm dt)
      values
  in new Num_by_nom.dataset_group gname bars


(** {1 Stacked barcharts} ****************************************)


class stacked_barchart_dataset dashes ?(width=Length.Pt 1.) ?(color=gray)
  name values =

  let pos = List.filter (fun v -> v >= 0.) (Array.to_list values)
  and neg = List.filter (fun v -> v < 0.) (Array.to_list values) in

  let min_val = List.fold_left (+.) 0. neg
  and max_val = List.fold_left (+.) 0. pos in

object(self)

  inherit Num_by_nom_dataset.dataset name

  val style = { line_color = black;
		line_dashes = dashes;
		line_width = width; }


  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst in
    let fn start_y value =
      let y_min = tr start_y
      and y_max = tr (start_y +. value) in
	fill_rectangle ctx ~color
	  (rectangle ~x_min:x ~x_max:(x +. width)
	     ~y_min ~y_max);
	draw_line ctx ~style [point x y_min;
			      point x y_max;
			      point (x +. width) y_max;
			      point (x +. width) y_min;
			      point x y_min;];
	(start_y +. value) in
      ignore (List.fold_left fn 0. neg);
      ignore (List.fold_left fn 0. pos)

end


let stacked_barchart_dataset dashes ?(width=Length.Pt 1.) ?(color=gray)
    name data =
  new stacked_barchart_dataset dashes ~width ~color name data


let stacked_barchart_datasets dashes ?(width=Length.Pt 1.) ?(color=gray)
    ?(gname = "") values =
  let bars = List.map
    (fun (nm,dt) ->
       new stacked_barchart_dataset dashes ~width ~color nm dt) values in
    new Num_by_nom.dataset_group gname bars


(* EOF *)
