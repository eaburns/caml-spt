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

type measurement =
  | In of float
  | Cm of float
  | Px of int
  | Pt of float

(* numeric conversion stuff *)

let points_to_cm pval =
  (** converts a number of points into corresponding centimeters measure *)
  pval /. points_per_centimeter

let points_to_in pval =
  (** converts a number of points into corresponding inches measure *)
    pval /. (points_per_centimeter /. centimeters_per_inch)

let points_to_pixels pval =
  truncate (pval /. points_per_centimeter *. pixels_per_centimeter)

let pixels_to_points pval =
  (float_of_int pval) /. pixels_per_centimeter *. points_per_centimeter

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
  truncate (cms *. pixels_per_centimeter)

let in_to_cm inches =
  inches *. centimeters_per_inch

let in_to_points inches =
  cm_to_points (in_to_cm inches)

let in_to_pixels inches =
  cm_to_pixels (in_to_cm inches)


(* exposed functionality *)

let measure_to_type measure =
  match measure with
    | In _ -> Inches
    | Cm _ -> Centimeters
    | Px _ -> Pixels
    | Pt _ -> Points


let same_type measure1 measure2 =
  (measure_to_type measure1) = (measure_to_type measure2)


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


let cm_to_flt cm =
  match cm with
    | Cm f -> f
    | _ -> failwith "cm wasn't in centimeters"


let in_to_flt inches =
  match inches with
    | In f -> f
    | _ -> failwith "inches wasn't in centimeters"


let pixels_to_int px =
  match px with
    | Px i -> i
    | _ -> failwith "px wasn't in pixels"


let points_to_flt pt =
  match pt with
    | Pt f -> f
    | _ -> failwith "pt wasn't in pixels"



(* EOF *)
