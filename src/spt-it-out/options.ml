(** Handling of options.

    @author eaburns
    @since 2010-06-19
*)

open Printf

type t =
  | Bool of (int -> bool -> unit)
  | Number of (int -> float -> unit)
  | Ident of (int -> string -> unit)
  | String of (int -> string -> unit)
  | List of (int -> Sexpr.sexpr list -> unit)
  | Expr of (int -> Sexpr.sexpr -> unit)


let option_error line opt = function
  | Bool _ ->
      failwith (sprintf "line %d: Expected a boolean after %s" line opt)
  | Number _ ->
      failwith (sprintf "line %d: Expected a number after %s" line opt)
  | Ident _ ->
      failwith (sprintf "line %d: Expected an identifier after %s" line opt)
  | String _ ->
      failwith (sprintf "line %d: Expected a string after %s" line opt)
  | List _ ->
      failwith (sprintf "line %d: Expected a list after %s" line opt)
  | Expr _ ->
      failwith (sprintf "line %d: Expected an expression after %s" line opt)


let rec handle opt_spec exprs =
  (** [handle opt_spec lst] handles parsing of options given a
      specification. *)
  try
    match exprs with
      | [] -> ()
      | Sexpr.Ident(line, opt) :: (Sexpr.Number (l, n) as exp) :: tl ->
	  begin match List.assoc opt opt_spec with
	    | Number f -> f l n
	    | Expr f -> f l exp
	    | x -> option_error line opt x
	  end;
	  handle opt_spec tl
      | Sexpr.Ident(line, opt) :: (Sexpr.Ident (l, n) as exp) :: tl ->
	  begin match List.assoc opt opt_spec with
	    | Ident f -> f l n
	    | Bool f when (n = "true" || n = "false") ->
		f l (bool_of_string n);
	    | Expr f -> f l exp
	    | x -> option_error line opt x
	  end;
	  handle opt_spec tl
      | Sexpr.Ident(line, opt) :: (Sexpr.String (l, n) as exp) :: tl ->
	  begin match List.assoc opt opt_spec with
	    | String f -> f l n
	    | Expr f -> f l exp
	    | x -> option_error line opt x
	  end;
	  handle opt_spec tl
      | Sexpr.Ident(line, opt) :: (Sexpr.List (l, n) as exp) :: tl ->
	  begin match List.assoc opt opt_spec with
	    | List f -> f l n
	    | Expr f -> f l exp
	    | x -> option_error line opt x
	  end;
	  handle opt_spec tl
      | Sexpr.Ident(line, opt) :: tl ->
	  failwith (sprintf "line %d: Unknown option %s" line opt)
      | Sexpr.Number (line, _) :: tl ->
	  failwith (sprintf "line %d: Expected options but got a number" line)
      | Sexpr.String (line, _) :: tl ->
	  failwith (sprintf "line %d: Expected options but got a string" line)
      | Sexpr.List (line, _) :: tl ->
	  failwith (sprintf "line %d: Expected options but got a list" line)
  with Not_found ->
    begin match List.hd exprs with
      | Sexpr.Ident (line, opt) ->
	  failwith (sprintf "line %d: Unknown option %s" line opt)
      | _ -> assert false
    end

let eval_length = function
    (** [eval_length l] evaluates a length. *)
  | Sexpr.List (_, Sexpr.Ident (_, "in") :: Sexpr.Number (_, i) :: []) ->
      Length.In i
  | Sexpr.List (_, Sexpr.Ident (_, "cm") :: Sexpr.Number (_, c) :: []) ->
      Length.Cm c
  | Sexpr.List (_, Sexpr.Ident (_, "pt") :: Sexpr.Number (_, p) :: []) ->
      Length.Pt p
  | Sexpr.List (_, Sexpr.Ident (_, "px") :: Sexpr.Number (_, p) :: []) ->
      Length.Px (truncate p)
  | x -> failwith (sprintf "line %d: Malformed length" (Sexpr.line_number x))


let eval_color exp =
  (** [eval_color l] evaluates a color. *)
  let r = ref 0. and g = ref 0. and b = ref 0. and a = ref 1. in
  let opts = [
    ":r", Number (fun _ n -> r := n);
    ":g", Number (fun _ n -> g := n);
    ":b", Number (fun _ n -> b := n);
    ":a", Number (fun _ n -> a := n);
  ]
  in
    begin match exp with
      | Sexpr.List(_, lst) -> handle opts lst
      | x ->
	  failwith (sprintf "line %d: Malformed color" (Sexpr.line_number x))
    end;
    Drawing.color ~r:!r ~g:!g ~b:!b ~a:!a


let eval_legend_loc = function
    (** [eval_legend_loc expr] evaluates a legend location. *)
  | Sexpr.Ident (l, "upper-right") -> Legend.Upper_right
  | Sexpr.Ident (l, "upper-left") -> Legend.Upper_left
  | Sexpr.Ident (l, "lower-right") -> Legend.Lower_right
  | Sexpr.Ident (l, "lower-left") -> Legend.Lower_left
  | Sexpr.List (_, Sexpr.Ident(_, "at") :: Sexpr.Ident(l, txt_loc)
		  :: Sexpr.Number(_, x) :: Sexpr.Number(_, y) :: []) ->
      let txt_loc = match String.lowercase txt_loc with
	| "text-before" -> Legend.Text_before
	| "text-after" -> Legend.Text_after
	| _ -> failwith (sprintf
			   "line %d: Malformed text location %s"
			   l "try one of 'text-before' or 'text-after'")
      in Legend.At (txt_loc, x, y)
  | x -> failwith (sprintf "line %d: Malformed legend location"
		     (Sexpr.line_number x))


let set_once reference line name vl =
  (** [set_once reference line name vl] sets the reference if it is
      not already set.  Otherwise an error is reported. *)
  if !reference = None
  then reference := Some vl
  else failwith (sprintf "line %d: %s already specified" line name)

let string_option_ref opt r =
  opt, String (fun l s -> set_once r l opt s)

let number_option_ref opt r =
  opt, Number (fun l n -> set_once r l opt n)

let length_option_ref opt r =
  opt, Expr (fun l e -> set_once r l opt (eval_length e))

let dashes r =
  ":dashes",
  List (fun l ds ->
	  set_once r l ":dashes" (Array.of_list (List.map eval_length ds)))

let glyph r =
  ":glyph",
  String (fun l s -> set_once r l ":glyph" (Drawing.glyph_of_string s))

let color r = ":color", Expr (fun l e -> set_once r l ":color" (eval_color e))

let legend_loc r =
  ":legend-location",
  Expr (fun l e -> set_once r l ":legend-location" (eval_legend_loc e))
