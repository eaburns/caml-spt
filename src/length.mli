(**
    @author jtd7
    @since 2010-05-06
*)

type t = In of float | Cm of float | Px of int | Pt of float

type units = Inches | Centimeters | Pixels | Points

val as_in : t -> float

val as_cm : t -> float

val as_px_float : t -> float

val as_px : t -> int

val as_pt : t -> float

val to_string : t -> string

val convert : t -> units -> float

val get_type : t -> units

(* EOF *)
