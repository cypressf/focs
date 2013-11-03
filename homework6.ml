
(* CODE FOR HOMEWORK 6
 *
 * Deterministic Turing machines
 *
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

let starting_config m w = fail "Function starting_config not implemented"

let accepting_config m c = fail "Function accepting_config not implemented"

let rejecting_config m c = fail "Function rejecting_config not implemented"

let halting_config m c = fail "Function halting_config not implemented"

let step_config m c = fail "Function step_config not implemented"

let run m w = fail "Function run not implemented"



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

let turing_DFA dfa = fail "Function turing_DFA not implemented"


let binary_sum () = fail "Function binary_sum not implemented"

