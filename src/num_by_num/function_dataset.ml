(**

    @author jtd7
    @since 2010-05-10
*)

open Num_by_num_dataset
open Drawing
open Geometry


class function_dataset dashes ?(width=Length.Pt 1.) ?(color=black) ?name funct =
  (** A line plot dataset. *)
object (self)
  inherit Line_dataset.line_dataset ?name ?midth ?color [||]

  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = width;
    }

  method draw ctx ~src ~dst =
    ()

  method draw_legend ctx ~x ~y =
    ()

  method legend_dimensions ctx =
    ()

  method avg_slope =
    nan

end

(* EOF *)
