(** Plotting of an arbitrary function.

    @author jtd7, eaburns
    @since 2010-05-10
*)

open Num_by_num_dataset
open Drawing
open Geometry


class function_dataset
  dashes ?(samples=200) ?(width=Length.Pt 1.) ?(color=black) ?name f =
  (** A line plot dataset. *)
object (self)
  inherit Num_by_num_dataset.dataset ?name ()

  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = width;
    }


  method private points src =
    (** [points src] computes the points *)
    let dx = (src.x_max -. src.x_min) /. (float samples) in
    let x = ref src.x_min in
    let pts = ref [] in
      while !x <= src.x_max do
	let y = f !x in
	  pts := (point ~x:!x ~y) :: !pts;
	  x := !x +. dx;
      done;
      List.rev !pts


  method dimensions =
    (** [dimensions] gets the dimensions.  Return an 'inverse
	rectangle' so that this *should* have no effect on the
	dimensions of the plot. *)
    rectangle ~x_min:infinity ~x_max:neg_infinity
      ~y_min:infinity ~y_max:neg_infinity


  method draw ctx ~src ~dst =
    let tr = rectangle_transform ~src ~dst in
      draw_line ctx ~box:dst ~style (List.map tr (self#points src))


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units Line_dataset.line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]

  method legend_dimensions ctx =
    (ctx.units Line_dataset.line_legend_length), (ctx.units width)

  method avg_slope = 0.

end

(* EOF *)
