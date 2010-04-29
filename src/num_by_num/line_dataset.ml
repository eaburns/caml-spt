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

  method draw ctx ~src ~dst rank =
    let tr = rectangle_transform ~src ~dst in
    let pts = ref [] in
      for i = (Array.length points) - 1 downto 0 do
	pts := (tr points.(i)) :: !pts
      done;
      draw_line ctx ~box:dst ~style:style !pts
end
