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
    ?(width=Length.Pt 1.) ?group values =
  (** [barchart_datasets ?use_color ?width ?group values] makes a
      group of bars datasets. *)
  let next_fill =
    if use_color
    then Factories.default_color_fill_pattern_factory ()
    else Factories.default_fill_pattern_factory () in
  let bars =
    List.map
      (fun (nm,dt) -> new barchart_dataset (next_fill ()) ~width nm dt)
      values
  in
    match group with
      | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
      | None -> bars


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
    ?(use_color=false) ?(width=Length.Pt 1.) ?group values =
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
  in
    match group with
      | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
      | None -> bars


(** {1 Stacked barcharts} ****************************************)


class stacked_barchart_dataset next_pattern ?name ?(width=Length.Pt 1.)
  values =
  let values = Array.to_list values in

  let pos = List.filter (fun (_,v) -> v >= 0.) values
  and neg = List.filter (fun (_,v) -> v < 0.) values in

  let min_val = List.fold_left (fun accum (_,value) -> accum +. value) 0. neg
  and max_val = List.fold_left (fun accum (_,value) -> accum +. value) 0. pos in
  let pos = List.map (fun (nm,value) -> value, next_pattern ()) pos
  and neg = List.map (fun (nm,value) -> value, next_pattern ()) neg
  and minor = (List.fold_left (fun accum (nm,_) -> nm ^ "," ^ accum) ""
		(List.rev values)) in
  let minor = String.sub minor 0 ((String.length minor - 1)) in
  let major_name = name in

object(self)

  inherit Num_by_nom_dataset.dataset
    (match major_name with None -> "" | Some n -> n)


  val style = { default_line_style with line_width = width; }

  method x_label_height ctx style width =
    match major_name with
      |	None -> fixed_width_text_height ctx ~style width minor
      | Some major -> ((fixed_width_text_height ctx ~style width minor) +.
			 (fixed_width_text_height ctx ~style width major))

  method draw_x_label ctx ~x ~y style ~width =
    let half_width = width /. 2. in
      match major_name with
	| None -> (draw_fixed_width_text
		     ctx ~x:(x +. half_width) ~y ~style ~width minor)
	| Some major ->(let height = self#x_label_height ctx style width in
			  draw_fixed_width_text ctx ~x:(x +. half_width)
			    ~y ~style ~width minor;
			  draw_fixed_width_text ctx ~x:(x +. half_width)
			    ~y:(y +. height /. 2.) ~style ~width major)

  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst
    and x_min = x
    and x_max = x +. width in
    let d start_y (value,fill) =
      let y_max = tr (start_y +. value)
      and y_min = tr start_y in
      let r = rectangle ~x_min ~x_max ~y_min ~y_max in
	draw_fill_pattern ctx r fill;
	draw_line ctx ~style [point x y_min;
			      point x y_max;
			      point (x +. width) y_max;
			      point (x +. width) y_min;
			      point x y_min;];
	start_y +. value in
      ignore (List.fold_left d 0. pos);
      ignore (List.fold_left d 0. neg)

end


let stacked_barchart_dataset ?(width=Length.Pt 1.) ?name
    ?(fill_factory=Factories.default_fill_pattern_factory ()) nm_data_array =
  new stacked_barchart_dataset fill_factory ?name ~width nm_data_array


let stacked_barchart_datasets ?(width=Length.Pt 1.) ?group
    ?(fill_factory=Factories.default_fill_pattern_factory())
    mjr_by_nm_data_array_list =
  let bars = List.map
    (fun  (name,nm_data_array) ->
       new stacked_barchart_dataset fill_factory ?name ~width nm_data_array)
    mjr_by_nm_data_array_list in

    match group with
      | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
      | None -> bars


(** {1 Stacked barcharts} ****************************************)


class layered_barchart_dataset next_pattern ?name ?(width=Length.Pt 1.)
  values =

  let values = Array.to_list values
  and neg_compare (_,v1) (_,v2) = compare v1 v2
  and pos_compare (_,v1) (_,v2) = compare v2 v1 in
  let pos = List.sort pos_compare (List.filter (fun (_,v) -> v >= 0.) values)
  and neg = List.sort neg_compare (List.filter (fun (_,v) -> v < 0.) values) in

  let min_val = List.fold_left (fun accum (_,value) -> accum +. value) 0. neg
  and max_val = List.fold_left (fun accum (_,value) -> accum +. value) 0. pos in
  let pos = List.map (fun (nm,value) -> nm,value, next_pattern ()) pos
  and neg = List.map (fun (nm,value) -> nm,value, next_pattern ()) neg in
  let minor = (List.fold_left (fun accum (nm,_,_) -> nm ^ "," ^ accum) ""
		(List.rev (neg @ pos))) in
  let minor = String.sub minor 0 ((String.length minor - 1)) in
  let major_name = name in
object(self)

  inherit Num_by_nom_dataset.dataset
    (match major_name with None -> "" | Some n -> n)


  val style = { default_line_style with line_width = width; }


  method x_label_height ctx style width =
    match major_name with
      |	None -> fixed_width_text_height ctx ~style width minor
      | Some major -> ((fixed_width_text_height ctx ~style width minor) +.
			 (fixed_width_text_height ctx ~style width major))

  method draw_x_label ctx ~x ~y style ~width =
    let half_width = width /. 2. in
      match major_name with
	| None -> (draw_fixed_width_text
		     ctx ~x:(x +. half_width) ~y ~style ~width minor)
	| Some major ->(let height = self#x_label_height ctx style width in
			  draw_fixed_width_text ctx ~x:(x +. half_width)
			    ~y ~style ~width minor;
			  draw_fixed_width_text ctx ~x:(x +. half_width)
			    ~y:(y +. height /. 2.) ~style ~width major)


  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let offset = (width /. 10.) in
    let tr = range_transform ~src ~dst
    and max_offset = max (List.length pos) (List.length neg) in
    let width = width -. ((float max_offset) *. offset) in
    let d x_min (_,value,fill) =
      let x_max = x_min +. width
      and y_max = tr value
      and y_min = tr 0. in
      let r = rectangle ~x_min ~x_max ~y_min ~y_max in
	draw_fill_pattern ctx r fill;
	draw_line ctx ~style [point x_min y_min;
			      point x_min y_max;
			      point x_max y_max;
			      point x_max y_min;
			      point x_min y_min;];
	x_min +. offset in
      ignore (List.fold_left d x pos);
      ignore (List.fold_left d x neg)

end


let layered_barchart_dataset ?(width=Length.Pt 1.) ?name
    ?(fill_factory=Factories.default_fill_pattern_factory ()) nm_data_array =
  new layered_barchart_dataset fill_factory ?name ~width nm_data_array


let layered_barchart_datasets ?(width=Length.Pt 1.) ?group
    ?(fill_factory=Factories.default_fill_pattern_factory())
    nm_data_array_list =
  let bars = List.map
    (fun (name,nm_data_array) ->
       new layered_barchart_dataset fill_factory ?name ~width nm_data_array)
    nm_data_array_list in
    match group with
      | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
      | None -> bars


(* EOF *)
