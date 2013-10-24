
(* 
 * CODE FOR HOMEWORK 4
 * Cypress Frankenfeld
 * Worked with Julian and Brendan
 *)


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

let fail s = raise (Failure s)


(*
 * A function to perform depth first search in a tree
 * created from a transition relation.
 *
 * dfs trans init goal   returns true if there is a node N in the 
 *       tree created by the transition relation 'trans' starting
 *       at the initial node 'init' such that 'goal N' is true
 *
 * The maximum search depth is controlled by the integer in reference
 * cell 'dfs_threshold'. Update that value from the shell if you want
 * to search deeper in the tree. 
 * (You shouldn't need to unless you create your own rather involved 
 * examples.) 
 *
 *)

let dfs_threshold = ref 1000

let dfs trans init goal = 
  let rec loop seen stack = 
    match stack with
      [] -> false
    | (curr,d) :: stack' when d < !dfs_threshold 
                           && not (List.mem curr seen) -> 
           if (goal curr) then true
	   else loop (curr::seen) ((List.map (fun x -> (x,d+1)) 
                                                      (trans curr))@stack')
    | (_,d) :: _ when d >= !dfs_threshold -> fail "DFS abort threshold reached"
    | _:: stack' -> loop seen stack'
  in loop [] [(init,0)]



(*
 * The type for PDAs
 * 
 *)

type 'a pda = {states : 'a list;
               input_alph : char list;
               delta : (('a * char option * char) * ('a * char list)) list;
               start : 'a;
               final : 'a list;
               stack_alph : char list;
               bottom : char}


(* 
 * IMPLEMENT THESE FUNCTIONS FOR QUESTION 3
 *
 *)

let initial_config pda w =
  (pda.start, explode w, [pda.bottom])

let accepting_config pda cfg = match cfg with
  | (_, c::cs, _) -> false
  | (state, [], _) when List.exists (fun item -> item = state) pda.final  -> true
  | _ -> false


let apply_transform delta cfg =
  let ((delta_state, input_symbol, stack_top), (new_state, push_stack)) = delta in
  let (cfg_state, symbols, stack_head::stack_tail) = cfg in
  if cfg_state = delta_state && stack_head = stack_top then
    if input_symbol = None then
      [(new_state, symbols, push_stack@stack_tail)]
    else if symbols = [] then
      []
    else
      let symbol_head::symbol_tail = symbols in
      if input_symbol = Some symbol_head then
        [(new_state, symbol_tail, push_stack@stack_tail)]
      else 
        []
  else
    []

let step_config pda cfg =
  List.fold_right (fun delta transformed_configs -> (apply_transform delta cfg)@transformed_configs ) pda.delta []

let accept pda w = 
  dfs (step_config pda) (initial_config pda w) (accepting_config pda)



(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings alphabet n = 
  let rec mapCons c = List.map (fun y -> c::y)  in
  let rec mapConsSet alphabet l = 
    List.fold_right (fun c -> List.append (mapCons c l)) alphabet []  in
  let rec strings' n =
    if (n<=0) then [[]]
    else [] :: mapConsSet alphabet (strings' (n-1))
  in List.map implode (strings' n)


(* 
 *  Compute the language of a PDA, restricted to inputs of length <= n
 *   language(dfa,n) returns a list of strings accepted by dfa
 *   printLanguage(dfa,n) prints the strings accepted by dfa
 *
 *)

let language pda n = 
  List.filter (accept pda) (strings pda.input_alph n)

let printLanguage pda n = 
  List.iter (fun s -> Printf.printf "  %s\n" (if (s="") then "<empty>" else s))
              (language pda n)


(*
 * Some sample PDAs
 *
 *)


let anban = { states = ["q1"; "q2"; "q3"];
              input_alph = ['a';'b'];
              delta = [(("q1", Some 'a', '_'), ("q1", ['.'; '_']));
		       (("q1", Some 'a', '.'), ("q1", ['.'; '.']));
		       (("q1", Some 'b', '_'), ("q2", ['_']));
		       (("q1", Some 'b', '.'), ("q2", ['.']));
		       (("q2", Some 'a', '.'), ("q2", []));
		       (("q2", None, '_'), ("q3", ['_']))];
	      start = "q1";
	      final = ["q3"];
	      stack_alph = ['.'; '_'];
	      bottom = '_' }

let anbn = { states = ["q1"; "q2"; "q3"];
              input_alph = ['a';'b'];
              delta = [(("q1", Some 'a', '_'), ("q1", ['.'; '_']));
		       (("q1", Some 'a', '.'), ("q1", ['.'; '.']));
		       (("q1", None, '_'), ("q2", ['_']));
		       (("q1", None, '.'), ("q2", ['.']));
		       (("q2", Some 'b', '.'), ("q2", []));
		       (("q2", None, '_'), ("q3", ['_']))];
	      start = "q1";
	      final = ["q3"];
	      stack_alph = ['.'; '_'];
	      bottom = '_' }

let pal = { states = ["q1"; "q2"; "q3"];
            input_alph = ['a'; 'b'];
            delta = [(("q1", Some 'a', '_'), ("q1", ['a'; '_']));
		     (("q1", Some 'a', 'a'), ("q1", ['a'; 'a']));
		     (("q1", Some 'a', 'b'), ("q1", ['a'; 'b']));
		     (("q1", Some 'b', '_'), ("q1", ['b'; '_']));
		     (("q1", Some 'b', 'a'), ("q1", ['b'; 'a']));
		     (("q1", Some 'b', 'b'), ("q1", ['b'; 'b']));
		     (("q1", None, '_'), ("q2", ['_']));
		     (("q1", None, 'a'), ("q2", ['a']));
		     (("q1", None, 'b'), ("q2", ['b']));
		     (("q2", Some 'a', 'a'), ("q2", []));
		     (("q2", Some 'b', 'b'), ("q2", []));
		     (("q2", None, '_'), ("q3", ['_']))];
            start = "q1";
            final = ["q3"];
            stack_alph = ['a'; 'b'; '_'];
            bottom = '_' }

let samenum = { states = ["q1"; "q2"];
		input_alph = ['a'; 'b'];
		delta = [(("q1", Some 'a', '_'), ("q1", ['a'; '_']));
			 (("q1", Some 'a', 'a'), ("q1", ['a'; 'a']));
			 (("q1", Some 'a', 'b'), ("q1", []));
			 (("q1", Some 'b', '_'), ("q1", ['b'; '_']));
			 (("q1", Some 'b', 'a'), ("q1", []));
			 (("q1", Some 'b', 'b'), ("q1", ['b'; 'b']));
			 (("q1", None, '_'), ("q2", ['_']))];
		start = "q1";
		final = ["q2"];
		stack_alph = ['a'; 'b'; '_'];
		bottom = '_' };;


(* step_config anbn ("q1", ['a'; 'b'], ['_']);;
step_config anbn ("q2", ['b'], ['.'; '_']);;
step_config anbn ("q2", ['b'], ['_']);;
step_config anbn ("q3", ['a'], ['_']);;
step_config pal ("q1", ['a';'b';'a'], ['a'; '_']);;
step_config pal ("q2", ['a';'b';'a'], ['a'; '_']);;
step_config pal ("q2", ['a';'b';'a'], ['b'; '_']);; *)

(* accept anbn "abbbab";;
accept anbn "abb";;
accept anbn "aabb";;
accept anbn "aabbb";;
accept anbn "aab";;
accept anbn "aaaabbbb";;
accept pal "";;
accept pal "a";;
accept pal "aa";;
accept pal "aabb";;
accept pal "abba";;
accept pal "abbbabaababbba";;
accept pal "abbabbabababba";;
 *)

