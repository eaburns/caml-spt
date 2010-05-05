(** resizing functionality and units conversion *)

let centimeters_per_inch = 2.54
let pixels_per_centimeter = (96. /. centimeters_per_inch)
and points_per_centimeter = (72. /. centimeters_per_inch)

let points_to_cm pval =
  (** converts a number of points into corresponding centimeters measure *)
  (float_of_int pval) /. points_per_centimeter


let points_to_in pval =
  (** converts a number of points into corresponding inches measure *)
  (float_of_int pval) /. (points_per_centimeter /. centimeters_per_inch)


let pixels_to_cm pval =
  (** converts a number of points into corresponding centimeters measure *)
  (float_of_int pval) /. pixels_per_centimeter


let pixels_to_in pval =
  (** converts a number of points into corresponding inches measure *)
  (float_of_int pval) /. (pixels_per_centimeter /. centimeters_per_inch)


let cm_to_points cms =
  cms *. points_per_centimeter


let in_to_points inches =
  cm_to_points (inches *. centimeters_per_inch)


let cm_to_pixels cms =
  cms *. pixels_per_centimeter


let in_to_pixels inches =
  cm_to_points (inches *. centimeters_per_inch)


(* EOF *)
