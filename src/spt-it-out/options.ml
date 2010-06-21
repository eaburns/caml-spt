(** Handling of options.

    @author eaburns
    @since 2010-06-19
*)

open Printf
open Evaluate

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
	  printf "line %d: Unknown option %s\n" line opt;
	  raise (Invalid_argument line)
      | Sexpr.Number (line, _) :: tl ->
	  printf "line %d: Expected options but got a number\n" line;
	  raise (Invalid_argument line)
      | Sexpr.String (line, _) :: tl ->
	  printf "line %d: Expected options but got a string\n" line;
	  raise (Invalid_argument line)
      | Sexpr.List (line, _) :: tl ->
	  printf "line %d: Expected options but got a list\n" line;
	  raise (Invalid_argument line)
  with Not_found ->
    begin match List.hd exprs with
      | Sexpr.Ident (line, opt) ->
	  printf "line %d: Unknown option %s\n" line opt;
	  raise (Invalid_argument line)
      | _ -> assert false
    end


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

let length_option_ref eval_rec env opt r =
  opt, Expr (fun l e -> match eval_rec env e with
	       | Length length -> set_once r l opt length
	       | x ->
		   printf "line %d: Expected length, get %s\n"
		     (Sexpr.line_number e) (value_to_string x);
		   raise (Invalid_argument (Sexpr.line_number e));)


let dashes eval_rec env r =
  ":dashes",
  List (fun l ds ->
	  set_once r l ":dashes"
	    (Array.of_list
	       (List.map (fun e -> match eval_rec env e with
			    | Evaluate.Length n -> n
			    | x ->
				printf "line %d: Expected length, got %s\n"
				  (Sexpr.line_number e) (value_to_string x);
				raise (Invalid_argument (Sexpr.line_number e)))
		  ds)))


let glyph r =
  ":glyph",
  String (fun l s -> set_once r l ":glyph" (Drawing.glyph_of_string s))

let color eval_rec env r =
  ":color", Expr (fun l e -> match eval_rec env e with
		    | Color color -> set_once r l ":color" color
		    | x ->
			printf "line %d: Expected color, got %s\n"
			  (Sexpr.line_number e) (value_to_string x);
			raise (Invalid_argument (Sexpr.line_number e));)

let legend eval_rec env r =
  ":legend",
  Expr (fun l e -> match eval_rec env e with
	  | Evaluate.Legend_loc loc -> set_once r l ":legend" loc
	  | x ->
	      printf "line %d: Expected legend location, got %s\n"
		(Sexpr.line_number e) (value_to_string x);
	      raise (Invalid_argument (Sexpr.line_number e)))


(** {1 Functions} ****************************************)

let eval_color eval_rec env line args =
  (** [eval_color eval_rec env line args] evaluates a color. *)
  let r = ref 0. and g = ref 0. and b = ref 0. and a = ref 1. in
  let opts = [
    ":r", Number (fun _ n -> r := n);
    ":g", Number (fun _ n -> g := n);
    ":b", Number (fun _ n -> b := n);
    ":a", Number (fun _ n -> a := n);
  ]
  in
    handle opts args;
    Color (Drawing.color ~r:!r ~g:!g ~b:!b ~a:!a)


let help_str_color =
  "(color [:r <number>] [:g <number>] [:b <number>] [:a <number>])\n\
Creates an RGBA color."


let eval_length make_len eval_rec env line = function
    (** [eval_in make_len eval_rec env line options] evaluates a
	length length. *)
  | Sexpr.Number (l, i) :: [] -> Length (make_len i)
  | x :: _ ->
      printf "line %d: Malformed length\n" (Sexpr.line_number x);
      raise (Invalid_argument (Sexpr.line_number x))
  | [] ->
      printf "line %d: Malformed length\n" line;
      raise (Invalid_argument line)


let eval_in eval_rec env line args =
  eval_length (fun x -> Length.In x) eval_rec env line args

let eval_cm eval_rec env line args =
  eval_length (fun x -> Length.Cm x) eval_rec env line args

let eval_pt eval_rec env line args =
  eval_length (fun x -> Length.Pt x) eval_rec env line args

let eval_px eval_rec env line args =
  eval_length (fun x -> Length.Px (truncate x)) eval_rec env line args


let functions = [
  "in", eval_in, "(in <number>)\nMakes an inches length";

  "cm", eval_cm, "(cm <number>)\nMakes an centimeters length";

  "pt", eval_pt, "(pt <number>)\nMakes an pounts length";

  "px", eval_px, "(px <number>)\nMakes an pixels length";

  "color", eval_color, help_str_color;
]
