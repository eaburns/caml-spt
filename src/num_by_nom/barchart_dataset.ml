(**

    @author jtd7
    @since 2010-05-26
*)

open Num_by_nom_dataset
open Drawing
open Geometry

type value =
    { name : string;
      data : float; }


class barchart_dataset dashes ?(width=Length.Pt 1.) ?(color=gray) name values =

  let min_val,max_val =
    (Array.fold_left
       (fun (amin,amax) ele ->
	  min amin ele.data,
	  max amax ele.data) (infinity,neg_infinity) values) in

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
    let tr = range_transform ~src ~dst
    and bar_width = width /. (float (Array.length values)) in
      Array.iteri
	(fun index value ->
	   let x_min = x +. bar_width *. (float index)
	   and x_max = x +. bar_width *. (float (index + 1))
	   and y_min = tr (min 0. value.data)
	   and y_max = tr (max 0. value.data) in
	     fill_rectangle ctx ~color  (rectangle ~x_min ~x_max ~y_min ~y_max);
	     draw_line ctx ~style [point x_min y_min;
				   point x_min y_max;
				   point x_max y_max;
				   point x_max y_min;
				   point x_min y_min;]) values

end


let barchart_dataset dashes ?(width=Length.Pt 1.) ?(color=gray) name values =
  new barchart_dataset dashes ~width ~color name
       (Array.of_list
	  (List.map (fun (a,b) -> { name = a; data = b}) values))

(* EOF *)
