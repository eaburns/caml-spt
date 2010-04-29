(** Lines and lines with errorbars.

    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Drawing
open Geometry

let dashes =
  (** The dash patterns for lines. *)
  [|
    [| |];
    [| 0.01; 0.01; |];
    [| 0.02; 0.02; |];
    [| 0.04; 0.01; |];
    [| 0.03; 0.02; 0.01; 0.02; |];
    [| 0.03; 0.01; 0.01; 0.01; 0.01; 0.01; |];
    [| 0.04; 0.005; 0.005; 0.005; 0.005; 0.005; 0.005; 0.005; |];
  |]


class line_dataset ?dash_pattern ?(width=0.002) ?(color=black) ?name points =
  (** A line plot dataset. *)
object (self)
  inherit points_dataset ?name points

  method style rank =
    (** [style rank] get the style of the line *)
    {
      line_color = color;
      line_dashes = begin match dash_pattern with
	| None -> dashes.(rank mod (Array.length dashes))
	| Some d -> d
      end;
      line_width = width;
    }

  method draw ctx ~src ~dst rank =
    let tr = rectangle_transform ~src ~dst in
    let pts = ref [] in
      for i = (Array.length points) - 1 downto 0 do
	pts := (tr points.(i)) :: !pts
      done;
      draw_line ctx ~box:dst ~style:(self#style rank) !pts
end


(** {1 Line points dataset} ****************************************)


class line_points_dataset
  ?dash_pattern ?width ?glyph ?radius ?color
  ?name points =
  (** A line with points plot dataset. *)
object
  inherit dataset ?name ()

  val line = new line_dataset ?dash_pattern ?width ?color points
  val scatter =
    new Scatter_dataset.scatter_dataset ?glyph ?radius ?color points

  method dimensions = rectangle_extremes scatter#dimensions line#dimensions

  method residual ctx ~src ~dst rank =
    rectangle_extremes
      (line#residual ctx ~src ~dst rank)
      (scatter#residual ctx ~src ~dst rank)

  method draw ctx ~src ~dst rank =
    line#draw ctx ~src ~dst rank;
    scatter#draw ctx ~src ~dst rank
end

