(** Evaluation of expressions for creating tree plots.

    @author eaburns
    @since 2010-07-05
*)

open Printf
open Evaluate


let eval_tree_node eval_rec env line ops =
  let color = ref None and children = ref [||] in
  let opt_spec = [
    Options.color eval_rec env color;
    ":succs",
    Options.Expr
      (fun l expr -> match expr with
	 | Sexpr.List (l, nodes) ->
	     let ch = List.map (eval_rec env) nodes
	     in children := Array.append !children (Array.of_list ch)
	 | x ->
	     let line = Sexpr.line_number x in
	       printf "line %d: expected a list of tree nodes" line;
	       raise (Invalid_argument line))

  ] in
    Options.handle eval_rec env opt_spec ops;
    let succs = Array.map (function
			     | Tree_node n -> n
			     | _ ->
				 printf "line %d: expected tree nodes" line;
				 raise (Invalid_argument line))
      !children
    in match !color with
      | None -> raise (Invalid_argument line)
      | Some c -> Tree_node { Tree_vis.color = c; Tree_vis.succs = succs }


let help_str_tree_node =
"(tree-node :color <color> [:succs (<tree node>+)]) makes a tree node"



let eval_sunburst_tree_plot eval_rec env line operands =
  (** [eval_sunburst_tree_plot eval_rec env line operands] evaluates a
      sunburst style tree plot. *)
  let module S = Sexpr in
  let title = ref None in
  let tree = ref None in
  let opt_spec = [
    Options.string_option_ref ":title" title;
    ":tree", Options.Expr (fun l e -> tree := Some (eval_rec env e));
  ] in
    Options.handle eval_rec env opt_spec operands;
    match !tree with
      | Some (Tree_node n) ->
	  Tree_plot (Tree_vis.plot ?title:!title Tree_vis.Sunburst.style n)
      | _ ->
	  printf "line %d: expected a tree node" line;
	  raise (Invalid_argument line)



let help_str_sunburst_tree_plot =
  "(sunburst-tree-plot :tree <tree>) makes a sunburst style tree plot."


let functions = [
  "sunburst-tree-plot", eval_sunburst_tree_plot, help_str_sunburst_tree_plot;
  "tree-node", eval_tree_node, help_str_tree_node;
]
