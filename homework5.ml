
(* 
 * CODE FOR HOMEWORK 5
 * Cypress Frankenfeld
 * Worked with Joe Gibson
 *)


let fail s = raise (Failure s)

(* 
 * String to Characters utility functions:
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode str = 
  let rec loop index result = 
    if (index<0) then result
    else loop (index-1) ((String.get str index)::result)
  in
    loop (String.length str - 1) []

let implode (cs) = 
  let str = String.create(List.length(cs))
  in (List.iteri (String.set str) cs; str)



(*
 * A function to perform depth first search in a tree
 * created from a transition relation.
 *
 * dfs quit trans init goal   
 *       returns true if there is a node N in the 
 *       tree created by the transition relation 'trans' starting
 *       at the initial node 'init' such that 'goal N' is true.
 *
 *       Quit search the current branch when 'quit N' is true 
 *
 * The 'quit' function is used to control the depth of the search. 
 * When 'quit N' returns true, it means that we have gone deep enough
 * and that we're acknowledging that we won't find the solution on
 * that branch.   
 *
 *)

let dfs quit trans init goal = 
  let rec loop seen stack = 
    match stack with
      [] -> false
    | curr :: stack' when not (List.mem curr seen) -> 
           if (goal curr) then true
	   else if (quit curr) then loop (curr::seen) stack'
	   else loop (curr::seen) ((trans curr)@stack')
    | _:: stack' -> loop seen stack'
  in loop [] [init]



(*
 * Some types to help us define CFGs (without epsilon production rules)
 *
 * symbol is either a terminal 'T char' or a nonterminal 'N string'
 *    (I'm using strings to describe nonterminals for added flexibility
 *
 * rule is the type of a non-epsilon production rule
 *    A rule can only be constructed with mk_rule that takes the 
 *    the nonterminal name 'A' and the list of symbols to produce 'w',
 *    and creates the rule 'A -> w', and it can only be used using
 *    get_rule that returns the components 'A' and 'w' of 'A -> w'
 *
 *)

type symbol = 
    T of char 
  | N of string


(* this module is simply to hide the implementation of the rule type 
 * so that the only way to create a rule is to use mk_rule below *)

module Rule : sig 
  type t
  val mk: string -> symbol list -> t
  val get: t -> string * symbol list 
  val strSymL : symbol list -> string
  val print_rule: Format.formatter -> t -> unit
end = struct
  type t = string * symbol list
  let mk s sl =
    match sl with
    | [] -> fail "Trying to create epsilon rule!"
    | _ -> (s,sl)
  let get r = r
  let strS (s) = "\""^s^"\""
  let strSym = function 
    | (T c) -> "T \'"^(Char.escaped c)^"\'"
    | (N s) -> "N "^(strS s)  
  let strSymL sl = "["^(String.concat "; " (List.map strSym sl))^"]"
  let print_rule ppf (s,sl) = 
    Format.fprintf ppf "(%s,%s)" (strS s) (strSymL sl)
end
;; #install_printer Rule.print_rule


type rule = Rule.t

(* this is used to create a non-epsilon production rule 
 * it fails if the symbol list is empty
 *)

let mk_rule : string -> symbol list -> rule = Rule.mk

(* this extracts the components of a production rule
 *)

let get_rule : rule -> string * symbol list = Rule.get




(*
 * The type for CFGs with no epsilon production rules
 *
 *)

type cfgNE = { nonterms_ne     : string list;
                terms_ne        : char list;
                rules_ne       : rule list;
                gen_empty_ne   : bool;
		start_ne       : string }



(* function to test if a symbol list is the sequence of terminals we seek *)

let found w = 
  let w' = List.map (fun c -> T c) (explode w)  in
  fun str -> str = w'


(* PROVIDE CODE FOR THIS FUNCTION FOR QUESTION 2 *)

let is_terminal = function
  | N s -> false
  | T s -> true

let rec find_steps beginning symbol end_symbols rules = 
  if (is_terminal symbol) then
    []
  else match rules with
    | [] -> []
    | hd::tl -> let (str, syms) = (get_rule hd) in
      if (N str) = symbol then
        [beginning @ syms @ end_symbols] @ find_steps beginning symbol end_symbols tl
      else
        find_steps beginning symbol end_symbols tl

let rec step2 used_syms syms rules = match syms with
  | [] -> []
  | hd::tl -> (find_steps used_syms hd tl rules) @ (step2 (used_syms@[hd]) tl rules)

let step cfgNE syms = (step2 [] syms cfgNE.rules_ne)



(* Function to search the derivation tree for a sequence of terminals
 * matching the provided string 
 * It searches until the sequence of symbols grows larger than the
 * string looked for.
 *)

let generate cfgNE w = 
  if (w = "") then cfgNE.gen_empty_ne
  else dfs (fun syms -> List.length syms > String.length w)
           (step cfgNE)
           [N cfgNE.start_ne]
           (found w)



(* 
 *  Compute the language of a CFG, restricted to inputs of length <= n
 *   language(cfg,n) returns a list of strings generated by cfg
 *   printLanguage(cfg,n) prints the strings generated by cfg
 *
 *)

let language cfgNE n = 
  let strings alphabet n = 
    let rec mapCons c = List.map (fun y -> c::y)  in
    let rec mapConsSet alphabet l = 
      List.fold_right (fun c -> List.append (mapCons c l)) alphabet []  in
    let rec strings' n =
      if (n<=0) then [[]]
      else [] :: mapConsSet alphabet (strings' (n-1))
    in List.map implode (strings' n)  in
  List.filter (generate cfgNE) (strings cfgNE.terms_ne n)

let printLanguage cfgNE n = 
  List.iter (fun s -> Printf.printf "  %s\n" (if (s="") then "<empty>" else s))
              (language cfgNE n)




(* 
 *  Some sample CFGs without any epsilon production rules
 *
 *)


(* The language {a^nb^n | n>=0}   *)

let anbn = { nonterms_ne = ["start"];
             terms_ne = ['a';'b'];
             rules_ne = [mk_rule "start" [T 'a'; T 'b'];
                         mk_rule "start" [T 'a'; N "start"; T 'b']];
             gen_empty_ne = true;
             start_ne = "start" }



(* The language {a^nb^nc^m | m>=0}   *)

let anbncm = { nonterms_ne = ["start"; "A"; "B"];
               terms_ne = ['a'; 'b'; 'c'];
	       rules_ne = [ mk_rule "start" [N "A"];
			    mk_rule "start" [N "B"];
			    mk_rule "start" [N "A"; N "B"];
			    mk_rule "A" [T 'a'; T 'b'];
			    mk_rule "A" [T 'a'; N "A"; T 'b'];
			    mk_rule "B" [T 'c'];
			    mk_rule "B" [T 'c'; N "B"]];
	       gen_empty_ne = true;
	       start_ne = "start" }




(*
 * The type for general CFGs 
 *
 *)

type cfg = { nonterms : string list;
	     terms    : char list;
	     rules    : (string * symbol list) list;
	     start    : string }



(* A sample grammar with language {a^nb^nb^ma^mc^p | m,n,p>=0}  *)

let anbnbmamcp = { nonterms = ["Start"; "A"; "B"; "C"];
		   terms = ['a';'b';'c'];
		   rules = [ ("Start", [N "A"; N "B"; N "C"]);
			     ("A", [T 'a'; N "A"; T 'b']);
			     ("A", []);
			     ("B", [T 'b'; N "B"; T 'a']);
			     ("B", []);
			     ("C", [T 'c'; N "C"]);
			     ("C", [])];
		   start = "Start" }
		           

(* PROVIDE CODE FOR THIS FUNCTION FOR QUESTION 5 *)
let build_rule str symbols = (str, symbols)
let build_rules str = List.map (build_rule str)
let extract_symbols = List.map (fun rules -> match rules with (_, symbols) -> symbols)

let remove_dups xs = List.fold_right (fun x xs' -> if (List.mem x xs') then xs' else x::xs') xs []

let rec replace_helper symbol prev_symbols symbols replaced_lists = match symbols with
  | [] -> replaced_lists
  | hd::tl ->
    if hd = symbol then
      replace_helper symbol (prev_symbols @ [hd]) tl (replaced_lists @ [prev_symbols @ tl])
    else
      replace_helper symbol (prev_symbols @ [hd]) tl replaced_lists

let replace symbol rule = let (key, symbols) = rule in build_rules key (replace_helper symbol [] symbols [])

(* Identify which nonterminals point to epsilons, and return a list of them. *)
let find_nonterm_epsilons rules =
  List.fold_right (fun rule keys ->
    let (key, value) = rule in 
    if value = [] then
      (N key) :: keys
    else
      keys
  ) rules []

let remove_epsilon_rules rules = 
  List.fold_right (fun rule rules' ->
    let (key, value) = rule in
    if value = [] then
      rules'
    else
      rule :: rules'
  ) rules []

let rec remove_e_for_symbol symbol rules = match rules with
  | [] -> []
  | (key, symbols)::rules' ->  (key,symbols) :: (remove_e_for_symbol symbol (rules' @ (replace symbol (key, symbols))) )

let rec remove_e_for_symbols symbols rules = match symbols with
  | [] -> rules
  | hd::tl -> remove_e_for_symbols tl (remove_e_for_symbol hd rules)


let mk_rules = List.map (fun rule -> let (key, value) = rule in mk_rule key value)

let eliminate_epsilon_rules cfg = 
  let new_rules = remove_dups (remove_e_for_symbols (find_nonterm_epsilons cfg.rules) (remove_epsilon_rules cfg.rules)) in
  if List.length (find_nonterm_epsilons new_rules) > 0 then
    { nonterms_ne = cfg.nonterms;
      terms_ne = cfg.terms;
      rules_ne = mk_rules (remove_epsilon_rules new_rules);
      gen_empty_ne = true;
      start_ne = cfg.start }
  else
      { nonterms_ne = cfg.nonterms;
      terms_ne = cfg.terms;
      rules_ne = mk_rules new_rules;
      gen_empty_ne = false;
      start_ne = cfg.start }

