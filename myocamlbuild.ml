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

      ocaml_lib ~extern:true ~dir:"+lacaml" "lacaml";
      ocaml_lib ~extern:true ~dir:"+cairo" "cairo";
      ocaml_lib ~extern:true ~dir:"+cairo" "cairo_lablgtk";
      ocaml_lib ~extern:true ~dir:"+lablgtk2" "lablgtk";
  | _ -> ()
end
