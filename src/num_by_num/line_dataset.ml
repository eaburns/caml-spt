(** Lines and lines with errorbars.

    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Drawing
open Geometry

let make_dash_factory dash_set () =
  (** [make_dash_factory dash_set ()] makes a dash pattern factory. *)
  let next = ref 0 in
  let n = Array.length dash_set in
    (fun () ->
       let d = dash_set.(!next) in
	 next := (!next + 1) mod n;
	 d)

let default_dash_factory =
  (** [default_dash_factory] gets the default dash factory builder. *)
  let default_dash_set =
    [|
      [| |];
      [| 0.01; 0.01; |];
      [| 0.02; 0.02; |];
      [| 0.04; 0.01; |];
      [| 0.03; 0.02; 0.01; 0.02; |];
      [| 0.03; 0.01; 0.01; 0.01; 0.01; 0.01; |];
      [| 0.04; 0.005; 0.005; 0.005; 0.005; 0.005; 0.005; 0.005; |];
    |]
  in make_dash_factory default_dash_set


let line_legend_length = 0.065
  (** The length of the line drawn in the legend. *)


class line_dataset dashes ?(width=0.002) ?(color=black) ?name points =
  (** A line plot dataset. *)
object (self)
  inherit points_dataset ?name points

  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = width;
    }

  method draw ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
    let pts = ref [] in
      for i = (Array.length points) - 1 downto 0 do
	pts := (tr points.(i)) :: !pts
      done;
      draw_line ctx ~box:dst ~style !pts


  method draw_legend ctx ~x ~y =
    let half_length = line_legend_length /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]


  method legend_dimensions _ = line_legend_length, width

end

let lines_points_dataset dashes glyph ?radius ?width ?color ?name points =
  (** [lines_points_dataset dashes glyph ?radius ?width ?color ?name
      points] makes a dataset that is a line with glyphs at each
      point. *)
  new composite_dataset ?name [
    new line_dataset dashes ?width ?color ?name points;
    new Scatter_dataset.scatter_dataset glyph ?radius ?color ?name points;
  ]
