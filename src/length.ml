(** Dealing with lengths in different units.

    @author eaburns
    @since 2010-05-05
*)


let cm_per_in = 2.54
let px_per_in = 96.
let pt_per_in = 72.
let px_per_cm = px_per_in /. cm_per_in
let pt_per_cm = pt_per_in /. cm_per_in
let px_per_pt = px_per_in /. pt_per_in

type t =
  | In of float
  | Cm of float
  | Px of int
  | Pt of float

let as_in = function
  | In i -> i
  | Cm c -> c /. cm_per_in
  | Px p -> (float p) /. px_per_in
  | Pt p -> p /. pt_per_in

let as_cm = function
  | In i -> i *. cm_per_in
  | Cm c -> c
  | Px p -> (float p) /. px_per_cm
  | Pt p -> p /. pt_per_cm

let as_px = function
  | In i -> truncate (i *. px_per_in)
  | Cm c -> truncate (c *. px_per_cm)
  | Px p -> p
  | Pt p -> truncate (p *. px_per_pt)

let as_pt = function
  | In i -> i *. pt_per_in
  | Cm c -> c *. pt_per_cm
  | Px p -> (float p) /. px_per_pt
  | Pt p -> p
