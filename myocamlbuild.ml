(** For including Cairo

    @author eaburns
    @since 2010-04-25
*)

open Ocamlbuild_plugin
open Command

let static = true;;

dispatch begin function
  | After_rules ->

      (* If `static' is true then every ocaml link in bytecode will
	 add -custom *)
      if static then flag ["link"; "ocaml"; "byte"] (A"-custom");

      ocaml_lib ~extern:true ~dir:"+cairo" "cairo";
(*       flag ["link"; "ocaml"; "byte"; "use_cairo"] *)
(*         (S[A "cairo.cma"; A"-cclib"; A"-lcairo"; ]); *)

(*       flag ["link"; "ocaml"; "native"; "use_cairo"] *)
(*         (S[A "cairo.cmxa"; A"-cclib"; A"-lcairo";]); *)

  | _ -> ()
end
