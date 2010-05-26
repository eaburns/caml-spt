(** The main file for spt-it-out.

    @author eaburns
    @since 2010-05-14
*)

open Verbosity

let verb_string = "Set the verbosity level"


let parse_args () =
  (** [parse_args ()] parses the command-line arguments. *)
  let file = ref ""
  and verb = ref verb_normal in
    Arg.parse [ "-v", Arg.Set_int verb, verb_string; ]
      (fun s ->
	 if !file <> ""
	 then invalid_arg "Specify only one input file (-h for help)"
	 else file := s)
      "./spt-it-out [-v <level 0-3>] <input file>";
    !file, !verb


let main () =
  let file, verb = parse_args () in
    Verb_level.set verb;
    let inch = open_in file in
    let ast = Sexpr.parse (Stream.of_channel inch) in
      ignore (Eval.eval Eval.init_env ast);
      close_in inch


let _ = main ()
