(** Analyze s-experssions, conversion to plotting language and execution.

    @author eaburns
    @since 2010-05-14
*)

open Printf
open Geometry
open Verbosity
open Evaluate

let eval_display eval_rec env line operands =
  (** [eval_display eval_rec env line operands] evaluates a display
      function.  This function displays the plots(s) using GTK.  The
      result is the final plots that was displayed. *)
  List.fold_left
    (fun _ p -> match eval_rec env p with
       | (`Num_by_num_plot p) as vl -> p#display; vl
       | (`Num_by_nom_plot p) as vl -> p#display; vl
       | x ->
	   failwith (sprintf "line %d: Expected plot got %s"
		       (Sexpr.line_number p) (value_to_string x)))
    `Unit operands


let eval_output eval_rec env line = function
    (** [eval_output eval_rec env line] evaluates an output function.
	This outputs the given plot to the specified file.  The result is
	the plot that was output. *)
  | Sexpr.String(_, filename) :: plot_expr :: [] ->
      begin match eval_rec env plot_expr with
	| (`Num_by_num_plot p) as vl -> p#output filename; vl
	| (`Num_by_nom_plot p) as vl -> p#output filename; vl
	| x ->
	    failwith (sprintf "line %d: Expected plot got %s"
			(Sexpr.line_number plot_expr) (value_to_string x))
      end
  | _ ->
      failwith (sprintf "line %d: Malformed output operation" line)


let functions =
  [
    "display", eval_display;
    "output", eval_output;
  ]
  @ Evaluate.functions
  @ Eval_data.functions
  @ Eval_num_by_num.functions
  @ Eval_num_by_nom.functions


let rec eval env expr = Evaluate.evaluate functions eval env expr
  (** [eval env expr] evaluates a spt-it-out expression. *)
