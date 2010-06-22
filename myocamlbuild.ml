(** OCaml build plugin to include the necessary external libs.

    @author eaburns
    @since 2010-04-25
*)

open Ocamlbuild_plugin
open Command

;;

dispatch begin function
  | After_rules ->
      flag ["link"; "ocaml"; "byte"] (A"-custom");

      ocaml_lib ~extern:true ~dir:"+cairo" "cairo";
      ocaml_lib ~extern:true ~dir:"+cairo" "cairo_lablgtk";
      ocaml_lib ~extern:true ~dir:"+lablgtk2" "lablgtk";

      ocaml_lib ~extern:true ~dir:"+lacaml" "lacaml";
      (* link with fortran when using lacaml. *)
      flag ["link"; "ocaml"; "use_lacaml"] (S[A "-cclib"; A "-lgfortran" ]);
  | _ -> ()
end
