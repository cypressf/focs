
(* CODE FOR HOMEWORK 6
 * Deterministic Turing Machines
 * Cypress Frankenfeld
 * iamcypress@gmail.com
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
    else loop (index-1) ((String.get str index)::result)  in
  loop (String.length str - 1) []

let implode (cs) = 
  let str = String.create(List.length(cs))  in
  (List.iteri (String.set str) cs; str)



(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type direction = Left | Right

type 'a turing = { states : 'a list;
		   input_alph : char list;
		   tape_alph : char list;
		   leftmost : char;
		   blank : char;
		   delta : ('a * char) -> ('a * char * direction);
		   start : 'a;
		   accept : 'a;
		   reject : 'a }


(*
 * Print a configuration (including newline) to standard output
 * 
 *)

let print_config m (u,q,v) = 
  let string cs =
    let str = String.make (List.length(cs) * 2) ' '  in
    let put i = String.set str (i*2)  in
    (List.iteri put cs; str)  in
  Printf.printf "  %s(%s) %s\n" (string u) q (string v)



(*
 * IMPLEMENT THE FOLLOWING FUNCTIONS FOR QUESTION 2
 *
 *)

let starting_config turing_machine str =
	([], turing_machine.start, turing_machine.leftmost::(explode str))

let accepting_config turing_machine c =
	let _, state, _ = c in
	turing_machine.accept = state

let rejecting_config turing_machine c =
	let _, state, _ = c in
	turing_machine.reject = state

let halting_config turing_machine c =
	(accepting_config turing_machine c) || (rejecting_config turing_machine c)


let pop_tail lst =
	let hd::rev_list = List.rev lst in (List.rev rev_list, hd)

let step_config turing_machine c =
	let (u, state, v) = c in
	
	let (new_state, write, direction), v_tail =
		if v = [] then
			turing_machine.delta (state, '_'), []
		else
			let v_head::v_tail = v in
			turing_machine.delta (state, v_head), v_tail
	in
	if direction = Right then
		(u@[write], new_state, v_tail)
	else
		let (u_new, new_tape_position) = pop_tail u in
		(u_new, new_state, new_tape_position::write::v_tail)


let run turing_machine str =
	let rec run_helper turing_machine config =
		print_config turing_machine config;
		if accepting_config turing_machine config then
			true
		else if rejecting_config turing_machine config then
			false
		else
			run_helper turing_machine (step_config turing_machine config)
	in
	let ans = run_helper turing_machine (starting_config turing_machine str) in
	if ans then
		Printf.printf "Accepting %s\n" str
	else
		Printf.printf "Rejecting %s\n" str;
	ans


(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
	     input_alph = ['a';'b'];
	     tape_alph = ['a';'b';'_';'>'];
	     blank = '_';
	     leftmost = '>';
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = fun (q,a) -> 
                       match (q,a) with
                       | ("start", 'a') -> ("start", 'a', Right)
     	               | ("start", 'b') -> ("q1", 'b', Right)
		       | ("start", '>') -> ("start", '>', Right)
		       | ("start", '_') -> ("acc", '_', Right)
		       | ("q1", 'a') -> ("rej", 'a', Right)
		       | ("q1", 'b') -> ("q1", 'b', Right)
		       | ("q1", '>') -> ("rej", '>', Right)
		       | ("q1", '_') -> ("acc", '_', Right)
		       | ("acc", sym) -> ("acc", sym, Right)
		       | ("rej", sym) -> ("rej", sym, Right)
		       | _ -> fail "Undefined transition" }

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     input_alph = ['a';'b'];
	     tape_alph = ['a';'b';'X';'_';'>'];
	     blank = '_';
	     leftmost = '>';
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = fun (q,a) -> 
	               match (q,a) with
		       | ("start", 'a') -> ("start", 'a', Right)
     	               | ("start", 'b') -> ("q1", 'b', Right)
		       | ("start", '>') -> ("start", '>', Right)
		       | ("start", '_') -> ("q2", '_', Right)
		       | ("start", 'X') -> ("rej", 'X', Right)
		       | ("q1", 'b') -> ("q1", 'b', Right)
		       | ("q1", '_') -> ("q2", '_', Right)
		       | ("q1", sym) -> ("rej", sym, Right)
		       | ("q2", '>') -> ("q3", '>', Right)
		       | ("q2", sym) -> ("q2", sym, Left)
		       | ("q3", 'X') -> ("q3", 'X', Right)
		       | ("q3", '_') -> ("acc", '_', Right)
		       | ("q3", 'a') -> ("q4", 'X', Right)
		       | ("q3", sym) -> ("rej", sym, Right)
		       | ("q4", 'a') -> ("q4", 'a', Right)
		       | ("q4", 'X') -> ("q4", 'X', Right)
		       | ("q4", 'b') -> ("q2", 'X', Right)
		       | ("q4", sym) -> ("rej", sym, Right)
		       | ("acc", sym) -> ("acc", sym, Right)
		       | ("rej", sym) -> ("rej", sym, Right)
		       | _ -> fail "Undefined transition" }

let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       input_alph = ['a';'b';'c'];
	       tape_alph = ['a';'b';'c';'X';'_';'>'];
	       blank = '_';
	       leftmost = '>';
	       start = "start";
	       accept = "acc";
	       reject = "rej";
	       delta = fun (q,a) -> 
	         match (q,a) with
		 | ("start", 'a') -> ("start", 'a', Right)
     	         | ("start", 'b') -> ("q1", 'b', Right)
		 | ("start", 'c') -> ("q6", 'c', Right)
		 | ("start", '>') -> ("start", '>', Right)
		 | ("start", '_') -> ("q2", '_', Right)
		 | ("start", 'X') -> ("rej", 'X', Right)
		 | ("q1", 'b') -> ("q1", 'b', Right)
		 | ("q1", 'c') -> ("q6", 'c', Right)
		 | ("q1", '_') -> ("q2", '_', Right)
		 | ("q1", sym) -> ("rej", sym, Right)
		 | ("q2", '>') -> ("q3", '>', Right)
		 | ("q2", sym) -> ("q2", sym, Left)
		 | ("q3", 'X') -> ("q3", 'X', Right)
		 | ("q3", '_') -> ("acc", '_', Right)
		 | ("q3", 'a') -> ("q4", 'X', Right)
		 | ("q3", sym) -> ("rej", sym, Right)
		 | ("q4", 'a') -> ("q4", 'a', Right)
		 | ("q4", 'X') -> ("q4", 'X', Right)
		 | ("q4", 'b') -> ("q5", 'X', Right)
		 | ("q4", sym) -> ("rej", sym, Right)
		 | ("q5", 'b') -> ("q5", 'b', Right)
		 | ("q5", 'X') -> ("q5", 'X', Right)
		 | ("q5", 'c') -> ("q2", 'X', Right)
		 | ("q5", sym) -> ("rej", sym, Right)
		 | ("q6", 'c') -> ("q6", 'c', Right)
		 | ("q6", '_') -> ("q2", '_', Right)
		 | ("q6", sym) -> ("rej", sym, Right)
		 | ("acc", sym) -> ("acc", sym, Right)
		 | ("rej", sym) -> ("rej", sym, Right)
		 | _ -> fail "Undefined transition" }



(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states_d :   'a list;
    	       alphabet_d : char list;
	       start_d :    'a;
   	       delta_d :    ('a * char * 'a) list;
	       final_d :    'a list}



(*
 * Some sample DFAs
 *
 * isolatedBs: all strings where every b is bracketed by a's
 * asThenBs: strings of a's followed by b's
 *
 *)

let isolatedBs =    
  {alphabet_d = ['a'; 'b'];   
   states_d = ["start"; "readb"; "sink"];
   start_d = "start";
   delta_d = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink"); 
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final_d = ["start";"readb"]}

let asThenBs =              
    {states_d = ["start"; "matchb"; "sink"];
     alphabet_d = ['a'; 'b'];
     start_d = "start";
     delta_d = [("start", 'a', "start");
              ("start", 'b', "matchb");
              ("matchb", 'a', "sink");
              ("matchb", 'b', "matchb");
              ("sink", 'a', "sink");
              ("sink", 'b', "sink")];
     final_d = ["start"; "matchb"]}


(* 
 * IMPLEMENT THE FOLLOWING FUNCTIONS FOR QUESTION 3
 *
 *)

let is_final dfa state = List.mem state dfa.final_d

(* Given a delta list from a dfa, a state, and an input character
	return the next state that you should transition to *)
let rec step_function delta (state, input) = match delta with
	| [] -> fail "Undefined transition"
	| (state', input', new_state)::tl ->
		if state' = state && input' = input then
			new_state
		else
			step_function tl (state, input)

let dfa_to_turing_delta dfa = fun (state, input) ->
	let next_state =
	if input = '_' then
		if is_final dfa state then
			"acc"
		else
			"rej"
	else if input = '>' && state = dfa.start_d then
		dfa.start_d
	else
		step_function dfa.delta_d (state, input)
	in (next_state, input, Right)

(* Note: this assumes that "acc" and "rej" are not already states of the dfa. *)
let turing_DFA dfa = 
	{states = dfa.states_d @ ["rej"; "acc"];
	input_alph = dfa.alphabet_d;
	tape_alph = dfa.alphabet_d @ ['_';'>'];
	blank = '_';
	leftmost = '>';
	start = dfa.start_d;
	accept = "acc";
	reject = "rej";
	delta = dfa_to_turing_delta dfa;}

(*
Note: I worked with Joe, Keely, and Abe on the following problem.

Steps:
1. Check to see if the input string is of the form x#y#z where x, y, z are
	strings using alphabet {0,1}, and x is at least 1 digit long
2. Check to see if x, y, z are the same number of digits by "checking off" their digits
	('0'->'a', and '1'->'b') one by one
3. Check to see if z + y = x. Use a tree of states to keep track of the sum for the current digit.
	If there is a carry value, when it loops back, start on the "carry" branch of the tree.
	If there is no carry value, when it loops back, start on the "no carry" branch of the tree.

	At the ends of the tree, check the value of the digit in the sum to see if it matches with the
	expected value given the current branch of the tree. If it does not match, reject.
	
	Some states of the tree end up being equivalent, so they are collapsed into one state. For example,
	adding a 0, then a 1 is equivalent to adding a 1, then a 0.
	
	Mark off each digit with an X to indicate that it has already been read.

4. Accept when there are no digits left to read. If there were invalid number of digits, or invalid
	addition, we would have caught it by now, so the addition is valid.

	Important note: accept even if there is a carry bit. This means that 111 + 111 = 110.
	Because we are fixing the length of the binary number. We cannot represent 111 + 111 = 1110
	The machine can be changed to reject such values by altering the line
	| ("c", '#') -> ("acc", '#', Left)
	to read
	| ("c", '#') -> ("rej", '#', Left)

5. Forgot to type the states. I guess the delta functions doesn't care about whether or not the state
	is actually in the list, so it was able to run just fine with no states. There should probably
	be more error checking that insures the states we transition to and the start, accept, and reject
	states are in the states list, and what we are writing and reading from the tape exists in the tape alphabet.
 *)

let binary_sum () =
  { states      = ["start";"q.5";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8";"q9";"q10";"nc";"c";"rew_nc";"rew_c";"0";"1";"#0";"0#0";"#0#0";"1#0";"#1#0";"#1";"1#1";"#1#1";"1c";"#1c";"1#1";"1#1c";"#1#1c";"acc";"rej"];
    input_alph  = ['0';'1'];
    tape_alph   = ['0';'1';'a';'b';'X';'_';'>'];
    blank       = '_';
    leftmost    = '>';
    start       = "start";
    accept      = "acc";
    reject      = "rej";
    delta       = fun (q,a) -> match (q,a) with
                  | ("start", '>') -> ("start", '>', Right)
                  | ("start", '0') -> ("q.5", '0', Right)
                  | ("start", '1') -> ("q.5", '1', Right)
                  | ("start", sym) -> ("rej", sym, Right)
                  | ("q.5", '0') -> ("q.5", '0', Right)
                  | ("q.5", '1') -> ("q.5", '1', Right)
                  | ("q.5", '#') -> ("q1", '#', Right)
                  | ("q.5", sym) -> ("rej", sym, Right)
                  | ("q1", '0') -> ("q1", '0', Right)
                  | ("q1", '1') -> ("q1", '1', Right)
                  | ("q1", '#') -> ("q2", '#', Right)
                  | ("q1", sym) -> ("rej", sym, Right)
                  | ("q2", '0') -> ("q2", '0', Right)
                  | ("q2", '1') -> ("q2", '1', Right)
                  | ("q2", '_') -> ("q3", '_', Right)
                  | ("q2", sym) -> ("rej", sym, Left)
                  | ("q3", '>') -> ("q4", '>', Right)
                  | ("q3", sym) -> ("q3", sym, Left)
                  | ("q4", 'a') -> ("q4", 'a', Right)
                  | ("q4", 'b') -> ("q4", 'b', Right)
                  | ("q4", '0') -> ("q5", 'a', Right)
                  | ("q4", '1') -> ("q5", 'b', Right)
                  | ("q4", '#') -> ("q9", '#', Right)
                  | ("q4", sym) -> ("rej", sym, Right)
                  | ("q5", '0') -> ("q5", '0', Right)
                  | ("q5", '1') -> ("q5", '1', Right)
                  | ("q5", '#') -> ("q6", '#', Right)
                  | ("q5", sym) -> ("q5", sym, Right)
                  | ("q6", 'a') -> ("q6", 'a', Right)
                  | ("q6", 'b') -> ("q6", 'b', Right)
                  | ("q6", '0') -> ("q7", 'a', Right)
                  | ("q6", '1') -> ("q7", 'b', Right)
                  | ("q6", sym) -> ("rej", sym, Right)
                  | ("q7", '0') -> ("q7", '0', Right)
                  | ("q7", '1') -> ("q7", '1', Right)
                  | ("q7", '#') -> ("q8", '#', Right)
                  | ("q7", sym) -> ("rej", sym, Right)
                  | ("q8", 'a') -> ("q8", 'a', Right)
                  | ("q8", 'b') -> ("q8", 'b', Right)
                  | ("q8", '0') -> ("q3", 'a', Right)
                  | ("q8", '1') -> ("q3", 'b', Right)
                  | ("q8", sym) -> ("rej", sym, Right)
                  | ("q9", 'a') -> ("q9", 'a', Right)
                  | ("q9", 'b') -> ("q9", 'b', Right)
                  | ("q9", '#') -> ("q10", '#', Right)
                  | ("q9", sym) -> ("rej", sym, Right)
                  | ("q10", 'a') -> ("q10", 'a', Right)
                  | ("q10", 'b') -> ("q10", 'b', Right)
                  | ("q10", '_') -> ("nc", '_', Left)
                  | ("q10", sym) -> ("rej", sym, Right)
                  | ("nc", 'X') -> ("nc", 'X', Left)
                  | ("nc", 'a') -> ("0", 'X', Left)
                  | ("nc", 'b') -> ("1", 'X', Left)
                  | ("nc", '#') -> ("acc", '#', Left)
                  | ("nc", sym) -> ("rej", sym, Left)
                  | ("0", 'a') -> ("0", 'a', Left)
                  | ("0", 'b') -> ("0", 'b', Left)
                  | ("0", '#') -> ("#0", '#', Left)
                  | ("0", sym) -> ("rej", sym, Left)
                  | ("#0", 'X') -> ("#0", 'X', Left)
                  | ("#0", 'a') -> ("0#0", 'X', Left)
                  | ("#0", 'b') -> ("1#0", 'X', Left)
                  | ("#0", sym) -> ("rej", sym, Left)
                  | ("0#0", 'a') -> ("0#0", 'a', Left)
                  | ("0#0", 'b') -> ("0#0", 'b', Left)
                  | ("0#0", '#') -> ("#0#0", '#', Left)
                  | ("0#0", sym) -> ("rej", sym, Left)
                  | ("#0#0", 'X') -> ("#0#0", 'X', Left)
                  | ("#0#0", 'a') -> ("rew_nc", 'X', Left)
                  | ("#0#0", sym) -> ("rej", sym, Left)
                  | ("rew_nc", '_') -> ("nc", '_', Left)
                  | ("rew_nc", sym) -> ("rew_nc", sym, Right)
                  | ("1#0", 'a') -> ("1#0", 'a', Left)
                  | ("1#0", 'b') -> ("1#0", 'b', Left)
                  | ("1#0", '#') -> ("#1#0", '#', Left)
                  | ("1#0", sym) -> ("rej", sym, Left)
                  | ("#1#0", 'X') -> ("#1#0", 'X', Left)
                  | ("#1#0", 'b') -> ("rew_nc", 'X', Left)
                  | ("#1#0", sym) -> ("rej", sym, Left)
                  | ("1", 'a') -> ("1", 'a', Left)
                  | ("1", 'b') -> ("1", 'b', Left)
                  | ("1", '#') -> ("#1", '#', Left)
                  | ("1", sym) -> ("rej", sym, Left)
                  | ("#1", 'X') -> ("#1", 'X', Left)
                  | ("#1", 'a') -> ("1#0", 'X', Left)
                  | ("#1", 'b') -> ("1#1", 'X', Left)
                  | ("#1", sym) -> ("rej", sym, Left)
                  | ("1#1", 'a') -> ("1#1", 'a', Left)
                  | ("1#1", 'b') -> ("1#1", 'b', Left)
                  | ("1#1", '#') -> ("#1#1", '#', Left)
                  | ("1#1", sym) -> ("rej", sym, Left)
                  | ("#1#1", 'X') -> ("#1#1", 'X', Left)
                  | ("#1#1", 'a') -> ("rew_c", 'X', Left)
                  | ("#1#1", sym) -> ("rej", sym, Left)
                  | ("c", 'X') -> ("c", 'X', Left)
                  | ("c", 'a') -> ("1", 'X', Left)
                  | ("c", 'b') -> ("1c", 'X', Left)
                  | ("c", '#') -> ("acc", '#', Left)
                  | ("c", sym) -> ("rej", sym, Left)
                  | ("1c", 'a') -> ("1c", 'a', Left)
                  | ("1c", 'b') -> ("1c", 'b', Left)
                  | ("1c", '#') -> ("#1c", '#', Left)
                  | ("1c", sym) -> ("rej", sym, Left)
                  | ("#1c", 'X') -> ("#1c", 'X', Left)
                  | ("#1c", 'a') -> ("1#1", 'X', Left)
                  | ("#1c", 'b') -> ("1#1c", 'X', Left)
                  | ("#1c", sym) -> ("rej", sym, Left)
                  | ("1#1c", 'a') -> ("1#1c", 'a', Left)
                  | ("1#1c", 'b') -> ("1#1c", 'b', Left)
                  | ("1#1c", '#') -> ("#1#1c", '#', Left)
                  | ("1#1c", sym) -> ("rej", sym, Left)
                  | ("#1#1c", 'X') -> ("#1#1c", 'X', Left)
                  | ("#1#1c", 'b') -> ("rew_c", 'X', Left)
                  | ("#1#1c", sym) -> ("rej", sym, Left)
                  | ("rew_c", '_') -> ("c", '_', Left)
                  | ("rew_c", sym) -> ("rew_c", sym, Right)
                  | _ -> fail "Undefined transition" }


(* halting_config asbs ([], "start", ['_']);;
halting_config asbs ([], "q1", ['>'; 'a'; '_']);;
halting_config asbs (['b'], "acc", ['>'; 'a'; '_']);;
halting_config asbs (['b'], "rej", ['>'; 'a'; '_']);; *)

(* step_config asbs ([], "start", ['_']);;
step_config asbs (['>'; 'a'], "start", ['b'; 'b']);;
step_config asbs (['>'; 'a'], "q1", ['a'; 'b']);;
step_config asbs (['>'; 'a'], "q1", ['b'; 'b']);;
step_config anbn (['>'; 'a'; 'b'], "q1", ['_']);;
step_config anbn (['>'; 'a'; 'b'], "q2", ['_']);;
step_config anbn (['>'], "q3", ['a';'b']);;
step_config anbn (['>';'X'], "q4", ['b']);; *)
