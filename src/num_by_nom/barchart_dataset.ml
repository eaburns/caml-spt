(**

    @author jtd7
    @since 2010-05-26
*)

open Num_by_nom_dataset
open Drawing
open Geometry

class barchart_dataset dashes ?(width=Length.Pt 1.) ?(color=gray) name value =
  let min_val = min 0. value
  and max_val = max 0. value in

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
    let y_min = tr (min 0. value)
    and y_max = tr (max 0. value) in
      fill_rectangle ctx ~color
	(rectangle ~x_min:x ~x_max:(x +. width) ~y_min ~y_max);
      draw_line ctx ~style [point x y_min;
			    point x y_max;
			    point (x +. width) y_max;
			    point (x +. width) y_min;
			    point x y_min;]

end


class barchart_errbar_dataset dashes ?(width=Length.Pt 1.) ?(color=gray)
  name values =

let mean, conf_interval = Statistics.mean_and_interval values in

let min_val = min 0. (mean -. conf_interval)
and max_val = max 0. (mean +. conf_interval) in

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
    let y_min = tr (min 0. mean)
    and y_max = tr (max 0. mean)
    and center = (width /. 2.) +. x in
      fill_rectangle ctx ~color
	(rectangle ~x_min:x ~x_max:(x +. width) ~y_min ~y_max);
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

let barchart_dataset dashes ?(width=Length.Pt 1.) ?(color=gray) name data =
  new barchart_dataset dashes ~width ~color name data


let barchart_datasets dashes ?(width=Length.Pt 1.) ?(color=gray)
    ?(gname = "") values =
  let bars = List.map
    (fun (nm,dt) ->
       new barchart_dataset dashes ~width ~color nm dt) values in
    new Num_by_nom.dataset_group gname bars


let barchart_errbar_dataset dashes ?(width=Length.Pt 1.) ?(color=gray)
    name data =
  new barchart_dataset dashes ~width ~color name data


let barchart_errbar_datasets dashes ?(width=Length.Pt 1.) ?(color=gray)
    ?(gname = "") values =
  let bars = List.map
    (fun (nm,dt) ->
       new barchart_errbar_dataset dashes ~width ~color nm dt) values in
    new Num_by_nom.dataset_group gname bars


(* EOF *)
