(** resizing functionality and units conversion *)

(* constants *)
let centimeters_per_inch = 2.54
let pixels_per_centimeter = (96. /. centimeters_per_inch)
and points_per_centimeter = (72. /. centimeters_per_inch)

(* type defs *)
type unit =
  | Inches
  | Centimeters
  | Pixels
  | Points
  | Raw

type measurement =
  | In of float
  | Cm of float
  | Px of float
  | Pt of float


(* numeric conversion stuff *)

let points_to_cm pval =
  (** converts a number of points into corresponding centimeters measure *)
  (float_of_int pval) /. points_per_centimeter

let points_to_in pval =
  (** converts a number of points into corresponding inches measure *)
  (float_of_int pval) /. (points_per_centimeter /. centimeters_per_inch)

let points_to_pixels pval =
  pval /. points_per_centimeter *. pixels_per_centimeter

let pixels_to_points pval =
  pval /. pixels_per_centimeter *. points_per_centimeter

let pixels_to_cm pval =
  (** converts a number of points into corresponding centimeters measure *)
  (float_of_int pval) /. pixels_per_centimeter

let pixels_to_in pval =
  (** converts a number of points into corresponding inches measure *)
  (float_of_int pval) /. (pixels_per_centimeter /. centimeters_per_inch)

let cm_to_inches cms =
  cms /. centimeters_per_inch

let cm_to_points cms =
  cms *. points_per_centimeter

let cm_to_pixels cms =
  cms *. pixels_per_centimeter

let in_to_cm inches =
  inches *. centimeters_per_inch

let in_to_points inches =
  cm_to_points (in_to_cm inches)

let in_to_pixels inches =
  cm_to_points (in_to_cm inches)


(* exposed functionality *)

let convert src_val units =
  (** [src_val] a measurement in some unit
      [units] target type of unit
      converts src to the unit type of units, returns a measurement *)
  match src_val with
    | In in_val ->
	(match units with
	   | Inches -> In in_val
	   | Centimeters -> Cm (in_to_cm in_val)
	   | Pixels -> Px (in_to_pixels in_val)
	   | Points -> Pt (in_to_points in_val))
    | Cm cm_val ->
	(match units with
	   | Inches -> In (cm_to_inches cm_val)
	   | Centimeters -> Cm cm_val
	   | Pixels -> Px (cm_to_pixels cm_val)
	   | Points -> Pt (cm_to_points cm_val))
    | Px px_val ->
	(match units with
	   | Inches -> In (pixels_to_in px_val)
	   | Centimeters -> Cm (pixels_to_cm px_val)
	   | Pixels -> Px px_val
	   | Points -> Pt (pixels_to_points px_val))
    | Pt pt_val ->
	(match units with
	   | Inches -> In (points_to_in pt_val)
	   | Centimeters -> Cm (points_to_cm pt_val)
	   | Pixels -> Px (points_to_pixels pt_val)
	   | Points -> Pt pt_val)


let get_val measure =
  (** Grabs the float value of a measurement *)
  match measure with
    | In flt -> flt
    | Cm flt -> flt
    | Px flt -> flt
    | Pt flt -> flt

(* EOF *)
