(** Plotting of an arbitrary function.

    @author jtd7, eaburns
    @since 2010-05-10
*)

open Num_by_num_dataset
open Drawing
open Geometry


class function_dataset
  dashes ?(samples=200) ?(line_width=Length.Pt 1.) ?(color=black) ?name f =
  (** A line plot dataset. *)
object (self)
  inherit Num_by_num_dataset.dataset ?name ()

  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = line_width;
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


  method mean_y_value src =
    let points = self#points src in
    let s, n =
      List.fold_left (fun (s, n) p -> s +. p.y, n + 1) (0., 0) points
    in s /. (float n), n


  method draw ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      draw_line ctx ~box:src ~tr ~style (self#points src)


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units Line_dataset.line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]

  method legend_dimensions ctx =
    (ctx.units Line_dataset.line_legend_length), (ctx.units line_width)

  method avg_slope = nan

end

(** {2 Scatter plot with best fit} ****************************************)

open Lacaml.Impl.D

let bestfit_dataset
    ~glyph ~dashes ?color ?line_width ?point_radius ?name points =
  let scatter =
    new Scatter_dataset.scatter_dataset
      glyph ?color ?point_radius ?name points in
  let xs = Mat.of_array (Array.map (fun p -> [| p.x; 1. |]) points) in
  let ys = Mat.of_array (Array.map (fun p -> [| p.y |]) points) in
    ignore (gelsd ~rcond:1e-4 xs ys);
    let m = ys.{1, 1} and b = ys.{2, 1} in
    let line =
      new function_dataset dashes ~samples:2 ?line_width ?color ?name
	(fun x -> x *. m +. b)
    in
      new composite_dataset ?name [scatter; line;]

let bestfit_datasets ?(uses_color=false)
    ?point_radius ?line_width name_by_point_list_list =
  let next_glyph = Factories.default_glyph_factory () in
  let next_dash = Factories.default_dash_factory () in
    if uses_color
    then (let next_color = Factories.default_color_factory () in
	    List.map (fun (name, point_list) ->
			bestfit_dataset
			  ~glyph:(next_glyph ())
			  ~dashes:(next_dash ())
			  ~color:(next_color())
			  ?line_width ?point_radius
			  ?name point_list) name_by_point_list_list)
    else
      List.map (fun (name, point_list) ->
		  bestfit_dataset ~glyph:(next_glyph ())
		    ~dashes:(next_dash ())
		    ?line_width ?point_radius ?name point_list)
	name_by_point_list_list

(* EOF *)
