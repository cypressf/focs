
(* Code for Homework 1 *****************************************)
(*
Cypress Frankenfeld
iamcypress@gmail.com
For this homework, I worked with Juliana Nazare and Joe Gibson.
*)



(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states :   'a list;
    	       alphabet : char list;
	       start :    'a;
   	       delta :    ('a * char * 'a) list;
	       final :    'a list}


(* 
 * String <-> characters utility functions:
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)


(*
 * Some error code
 * Call "transitionError" to report an error while looking for a 
 *   transition in the delta of a DFA
 *
 *)

exception DFAError of string

exception Unimplemented of string

let transitionError (input) = 
  raise (DFAError("Cannot transition on input "^(implode [input])))


(*
 * Some sample DFAs
 *
 *)


let isolatedBs =                (* language: all strings where every b *)
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "readb"; "sink"];
   start = "start";
   delta = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink"); 
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["start";"readb"]}


let asThenBs =                (* language: strings of a's followed by b's *)
    {states = ["eata"; "eatb"; "sink"];
     alphabet = ['a'; 'b'];
     start = "eata";
     delta = [("eata", 'a', "eata");
              ("eata", 'b', "eatb");
              ("eatb", 'a', "sink");
              ("eatb", 'b', "eatb");
              ("sink", 'a', "sink");
              ("sink", 'b', "sink")];
     final = ["eata"; "eatb"]}


let aStar =                    (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     (* language: all nonempty strings of b's *)
  {alphabet= ['a'; 'b'];
   states= ["start"; "ok"; "sink"];
   start= "start";
   delta = [("start", 'b', "ok");
            ("start", 'a', "sink");
            ("ok",    'b', "ok");
            ("ok",    'a', "sink");
            ("sink",  'b', "sink");
            ("sink",  'a', "sink")];
   final = ["ok"]}

let abaStar =              (* language: any number of ab's followed by a's *)
  {alphabet= ['a'; 'b'];
   states= ["astate"; "bstate"; "aonly"; "sink"];
   start= "astate";
   delta = [("astate", 'a', "bstate");
            ("astate", 'b', "sink");
            ("bstate", 'a', "aonly");
            ("bstate", 'b', "astate");
            ("aonly",  'a', "aonly");
            ("aonly",  'b', "sink");
            ("sink",   'a', "sink");
            ("sink",   'b', "sink")];
   final = ["astate"; "bstate"; "aonly"]}



(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings (alphabet, n) = 
  let rec mapCons (c, ls) = 
    match ls with
      [] -> []
    | l::ls' -> (c::l)::mapCons(c,ls')  in
  let rec mapConsSet (alphabet, l) = 
    match alphabet with
      [] -> []
    | c::cs -> mapCons(c,l) @ mapConsSet(cs,l)  in
  let rec mapImplode (css) = 
    match css with
      [] -> []
    | (cs::css) -> (implode cs)::mapImplode(css)  in
  let rec strings' (n) = 
    if (n<=0) then
      [[]]
    else let s = strings'(n-1) in
      [] :: mapConsSet(alphabet,s)
  in 
    mapImplode(strings'(n))




(*
 *  isFinal : 'a dfa * 'a -> bool
 *
 *    isFinal(dfa,q) should return true if and only if 'q' is a final state
 *    in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let isFinal (dfa,state) = 
	List.mem state dfa.final 



(* 
 *  transition : 'a dfa * 'a * char -> 'a
 *
 *    transition(dfa,q,a) should return the state obtained by reading input
 *    symbol 'a' in state 'q' in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)


let rec find_value (delta_list, state, input) = match delta_list with
  | [] -> state
  | (st, inpt, new_state)::_ when (st = state && inpt = input) -> new_state
  | _::others -> find_value (others, state, input)

let transition (dfa,state,input) = 
  find_value (dfa.delta, state, input)



(*
 *  extendedTransition : 'a dfa * 'a * char list -> 'a
 *
 *    extendedTransition(dfa,q,cs) should return the state obtained by
 *    reading the list of input symbols in 'cs' from state 'q' in the DFA
 *    'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let rec extendedTransition (dfa, state, cs) = match cs with
  | [] -> state
  | head::tail -> extendedTransition (dfa, transition(dfa, state, head), tail)


(*
 *  accept : 'a dfa * string -> bool
 *
 *    accept(dfa,input) should return true if and only the input string
 *    'input' is accepted by the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let accept (dfa, input) = 
  isFinal(dfa, extendedTransition(dfa, dfa.start, explode(input)))
  



(* 
 *  Compute the language of a DFA, restricted to inputs of length <= n
 *   language(dfa,n) returns a list of strings accepted by dfa
 *   printLanguage(dfa,n) prints the strings accepted by dfa
 *
 *)

let language (dfa, n) = 
  let candidates = strings(dfa.alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (accept(dfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)


let printLanguage (dfa,n) = 
  let rec printList (l) = 
    match l with 
      [] -> ()
    | s::ss -> (print_string "   ";
                if (s="") then
                  print_string "<empty>"
                else
                  print_string s; 
                print_newline(); 
                printList ss)
  in
    printList(language(dfa,n))

(* Excercise 3 *)

(* (a) Code a function member of type
'a * 'a list -> bool
where member(a,l) is true if and only if a is an element of l.
*)
let rec member (item, lst) = match lst with
  | [] -> false
  | head::tail when (head = item) -> true
  | head::tail -> member (item, tail)

(* (b) Code a function difference of type
'a list * 'a list -> 'a list
where difference(l1,l2) returns the list of all elements of l1 that are not elements
of l2. *)
let rec difference (list1, list2) = match list1 with
  | [] -> []
  | head::tail when member(head, list2) -> difference (tail, list2)
  | head::tail -> head :: difference (tail, list2)

(* (c) Code a function cross of type
'a list * 'b list -> ('a * 'b) list
where cross(l1,l2) returns the list of all pairs composed of an element of l1 paired
with an element of l2. *)

let rec getTuples (element, lst) = match lst with
  | [] -> []
  | head::tail -> (element, head) :: getTuples (element, tail) 

let rec cross (list1, list2) = match list1 with
  | [] -> []
  | head::tail -> getTuples(head, list2) @ cross(tail, list2)

(* (d) Code a function compl of type
'a dfa -> 'a dfa
where compl(d) returns a DFA accepting the complement of the language of DFA
d. *)
let compl dfa =
  {alphabet = dfa.alphabet;   
     states = dfa.states;
     start = dfa.start;
     delta = dfa.delta;
     final = difference (dfa.states, dfa.final)}

(* (e) Code a function inter of type1
'a dfa * 'b dfa -> ('a * 'b) dfa
where inter(d1,d2) returns the DFA accepting exactly the intersection of
the language of DFA d1 and the language of DFA d2 *)
let rec getDeltas (lst) = match lst with
  | [] -> []
  | ((a1,a2,a3), (b1,b2,b3)) :: tail when a2 = b2 -> ((a1, b1), a2, (a3, b3)) :: getDeltas(tail)
  | head :: tail -> getDeltas(tail)

let inter (dfa1, dfa2) = 
  {
    alphabet = dfa1.alphabet;   
    states = cross(dfa1.states, dfa2.states);
    start = (dfa1.start, dfa2.start);
    delta = getDeltas(cross(dfa1.delta, dfa2.delta));
    final = cross(dfa1.final, dfa2.final);
  }


(* (f) Code a function union of type
'a dfa * 'b dfa -> ('a * 'b) dfa
where union(d1,d2) returns the DFA accepting exactly the union of the language
of DFA d1 and the language of DFA d2.
 *)

let union (dfa1, dfa2) = 
  {
    alphabet = dfa1.alphabet;   
    states = cross(dfa1.states, dfa2.states);
    start = (dfa1.start, dfa2.start);
    delta = getDeltas(cross(dfa1.delta, dfa2.delta));
    final = cross(dfa1.final, dfa2.states) @ cross(difference(dfa1.states, dfa1.final), dfa2.final);
  }