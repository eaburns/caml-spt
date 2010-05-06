(**

    @author jtd7
    @since 2010-05-06
*)

val create_display :
  < draw : Drawing.context -> 'a; output : string -> unit;
    set_size : w:Length.t -> h:Length.t -> 'b; .. > ->
  string -> unit

(* EOF *)
