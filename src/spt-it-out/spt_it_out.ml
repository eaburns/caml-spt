(** The main file for spt-it-out.

    @author eaburns
    @since 2010-05-14
*)

let main () =
  let file = Sys.argv.(1) in
  let inch = open_in file in
  let ast = Sexpr.parse (Stream.of_channel inch) in
    ignore (Eval.eval Eval.init_env ast);
    close_in inch


let _ = main ()
