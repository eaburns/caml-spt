(** Lines with error bars.

    @author eaburns
    @since 2010-04-30
*)

open Geometry

type style = {
  dashes : float array;
  (* The dash pattern for the line. *)
  number : int;
  (* The number of the current line_errbar_dataset. *)
  count : int ref;
  (* A reference that counts the total number of associated
     line_errbar_datasets. *)
}


let line_errbar_factory next_dashes () =
  (** [line_errbar_factory next_dashes ()] gets a line and errorbar
      factory.  This helps choosing where the error bars reside. *)
  let count = ref 0 in
    (fun () ->
       incr count;
       {
	 dashes = next_dashes ();
	 number = !count - 1;
	 count = count;
       })


let next_location xrange ~num ~count cur =
  (** next_location xrange ~num ~count cur] gets the next error bar
      location after the value [cur]. *)
  let ngroups = 10. in
  let min = xrange.min and max = xrange.max in
  let numf = float num and countf = float count in
  let group_width = (max -. min) /. ngroups in
  let delta = group_width /. countf in
  let group_start = delta /. 2. in
  let group_offs = (group_start +. (delta *. numf)) /. group_width in
  let cur_group_num = floor (cur /. group_width) in
  let cur_group_offs = (cur /. group_width) -. cur_group_num in
  let group =
    if cur_group_offs >= group_offs
    then cur_group_num +. 1.
    else cur_group_num
  in (group *. group_width) +. (group_width *. group_offs)



let line_domain l =
  (** [line_domain l] gets the domain of the line. *)
  Array.fold_left (fun r pt ->
		     let min = if pt.x < r.min then pt.x else r.min
		     and max = if pt.x > r.max then pt.x else r.max
		     in range min max)
    (range infinity neg_infinity) l


let common_domain lines =
  (** [common_domain lines] get the domain that the given lines have
      in common. *)
  let domains = Array.map line_domain lines in
    Array.fold_left (fun cur d ->
		       let min = if d.min < cur.min then cur.min else d.min
		       and max = if d.max > cur.max then cur.max else d.max
		       in range min max)
      (range neg_infinity infinity) domains


let mean_line domain lines =
  (** [mean_line lines] get a line that is the mean of all of the
      given lines. *)
  let module Float_set = Set.Make(struct
				    type t = float
				    let compare (a:float) b = compare a b
				  end) in
  let min = domain.min and max = domain.max in
  let xs =
    Array.fold_left (fun set l ->
		       Array.fold_left (fun set pt ->
					  let x = pt.x in
					    if x >= min && x <= max
					    then Float_set.add x set
					    else set)
			 set l)
      Float_set.empty lines
  in
    Float_set.fold
      (fun x lst ->
	 let ys = Array.map (fun l -> interpolate l x) lines in
	 let mean = Statistics.mean ys in
	   (x, mean) :: lst)
      xs []


(*
let errbar_locations ~num ~total domain lines =
  (** [errbar_locations ~num ~total domain lines] get the x-locations
  of the error bars. *)
  let min = domain.min and max = domain.max in
  let cur_x = ref min in
  let intervals = ref [] in
    while !cur_x < max do
    done;
    !intervals
*)
