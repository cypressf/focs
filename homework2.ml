(*
Cypress Frankenfeld
iamcypress@gmail.com
For this homework, I worked with Juliana Nazare and Joe Gibson again.
*)

(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 * Everything from here to REGULAR EXPRESSIONS is from Homework 1
 *
 *)

type 'a dfa = {
  states :   'a list;
  alphabet : char list;
	start :    'a;
  delta :    ('a * char * 'a) list;
	final :    'a list
}

(* 
 * String to Characters utility functions:
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
 * Some error code call 
 * Call "transitionError" to report an error while looking for a 
 *   transition in the delta of a DFA
 *
 *)

exception DFAError of string

let transitionError (input) = 
  raise (DFAError("Cannot transition on input "^(implode [input])))



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
 * You'll need to put in working code for accept here (along with any
 * auxiliary functions it needs) for the code to work
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




(*************************************************************
 *
 *   REGULAR EXPRESSIONS
 *
 *)



(* 
 * Type for regular expressions 
 *
 *)

type regexp = 
    One
  | Zero
  | Char of char
  | Plus of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp


(*
 * Hook into the OCaml shell to print regular expressions nicely
 *
 *)

let print_regexp ppf rexp = 
  let rec parConcat (rexp) = 
    match rexp with
    | Plus (_,_) -> "("^(to_string rexp)^")"
    | _ -> to_string (rexp)
  and parStar (rexp) = 
    match rexp with
    | Plus (_,_) -> "("^(to_string rexp)^")"
    | Concat (_,_) -> "("^(to_string rexp)^")"
    | _ -> to_string (rexp)
  and to_string (rexp) = 
    match (rexp) with
      One -> "1"
    | Zero -> "0"
    | Char (c) -> Char.escaped c
    | Plus (rexp1, rexp2) -> (to_string rexp1)^"+"^(to_string rexp2)
    | Concat (rexp1,rexp2) -> (parConcat rexp1)^(parConcat rexp2)
    | Star (rexp1) -> (parStar rexp1)^"*"
  in Format.fprintf ppf "%s" (to_string rexp)
;;
#install_printer print_regexp


(* 
 * Some sample regular expressions
 *
 *)
let re1 = 
  Concat (Star (Char 'b'), Star (Concat (Char 'a', Star (Char 'b'))))

let re2 = 
  Concat (Star (Char 'a'), 
          Star (Concat (Char 'b', Concat(Char 'a',Star (Char 'a')))))

let re3 = Concat (Star (Char 'b'),
                  Concat (Plus (Char 'a', Char 'b'),
                          Star (Char 'b')))

let r1 = Concat (Char 'a', Char 'b')                           (* ab      *)
let r2 = Concat (Char 'a', Char 'c')                           (* ac      *)
let r3 = Concat (Char 'c', Char 'd')                           (* cd      *)       
let r4 = Concat (Char 'b', Star (Char 'a'))              (* ba*    *)        
let r = Plus (r1, Plus (r2, Plus (r3,r4)))                (* ab + ac + cd + ba*   *)    


(*
 * Massive massive ugliness hidden away in a module
 *  (you can pretty much ignore everything between 'module' and 
 *   'end' below)
 *
 * This is all a bunch of helper functions to help simplify a
 * regular expression so that we can reasonably compare two
 * regular expressions for equivalence.
 *
 * Did I mention it was ugly? It is.
 *
 * You don't need to understand this code.
 *
 * It's perfectly fine to simply assume that function 'simplify'
 * given below uses magic, per Arthur C. Clarke's definition.
 *
 *)

module SimplifyHelp : sig val simplify : regexp -> regexp end = 
  struct

    type regexp_alt = 
        A_One
      | A_Zero
      | A_Char of char
      | A_Plus of regexp_alt list
      | A_Concat of regexp_alt * regexp_alt
      | A_Star of regexp_alt

    let rec 
      regexp_to_alt (rexp) = 
        match rexp with
	  One -> A_One
	| Zero -> A_Zero
	| Char c -> A_Char c
	| Plus (rexp1, rexp2) -> A_Plus ((smash rexp1) @ (smash rexp2))
	| Concat (rexp1, rexp2) -> A_Concat (regexp_to_alt rexp1,
  					     regexp_to_alt rexp2)
	| Star rexp1 -> A_Star (regexp_to_alt rexp1)
    and 
      smash (rexp) = 
        match rexp with
	  Plus (rexp1, rexp2) -> (smash rexp1) @ (smash rexp2)
	| r -> [regexp_to_alt r]

    let rec collapseAPlus (l) = 
      match l with
	[] -> raise (Failure "trying to collapse A_Plus []")
      | [_] -> raise (Failure "trying to collapse A_Plus [_]")
      | [r1;r2] -> Plus (alt_to_regexp r1, alt_to_regexp r2)
      | r::rs -> Plus (alt_to_regexp r, collapseAPlus rs)

    and alt_to_regexp (rexp_alt) = 
      match rexp_alt with
	A_One -> One
      | A_Zero -> Zero
      | A_Char c -> Char c
      | A_Plus l -> collapseAPlus l
      | A_Concat (r1, r2) -> Concat (alt_to_regexp r1, alt_to_regexp r2)
      | A_Star (r1) -> Star (alt_to_regexp r1)

    let rec simplifyAlt (rexp_alt) =       
      let rec removeDup (l) = 
	match l with
	  [] -> []
	| r::rs -> if (List.mem r rs) 
        then removeDup rs
        else r::(removeDup rs)  in
      let rec simpAPlus l = 
	match l with
	  [] -> []
	| A_Zero::rs -> simpAPlus rs
	| (A_Plus subl)::rs -> simpAPlus(subl@rs)
	| r::rs -> (simp r)::simpAPlus rs 
      and simp (rexp_alt) = 
	match rexp_alt with
	  A_One -> A_One
	| A_Zero -> A_Zero
	| A_Char (c) -> A_Char (c)
	| A_Plus [] -> A_Zero
	| A_Plus [rexp] -> simp rexp
	| A_Plus l -> A_Plus (List.sort compare (removeDup (simpAPlus l)))
	| A_Concat (A_Zero, rexp2) -> A_Zero
	| A_Concat (rexp1, A_Zero) -> A_Zero
	| A_Concat (A_One, rexp2) -> simp rexp2
	| A_Concat (rexp1, A_One) -> simp rexp1
	| A_Concat (rexp1, rexp2) -> A_Concat (simp rexp1, simp rexp2)
	| A_Star (rexp1) -> A_Star (simp rexp1) in
      let rec loop (rexp) = 
	let s = simp (rexp) 
	in if (s = rexp) 
        then s
	else loop s
      in loop (rexp_alt)

    let simplify rexp = alt_to_regexp (simplifyAlt (regexp_to_alt rexp))

  end




(*
 * Simplify a regular expression
 * (gets rid of +0, and *0, *1, and r + r when r's are equivalent)
 *
 *   regexp -> regexp
 *)

let simplify (rexp) = SimplifyHelp.simplify rexp


(*
 * Check whether two regular expressions are equivalent 
 *
 *  regexp * regexp -> bool
 *)

let equivRE (r1, r2) = (simplify r1 = simplify r2)



(* 
 * Check if a regexp appears in a list using equivalence instead of =
 *
 *  regexp * regexp list -> bool
 *)

let rec memberRE (rexp, l) = 
  match l with
    [] -> false
  | r::rs -> equivRE (rexp,r) || memberRE (rexp,rs)


(*
 * Append two lists of regexps in such a way as to not produce duplicates
 * if an equivalent regexp appears in both lists
 *
 *  regexp list * regexp list -> regexp list
 *)

let rec unionRE (rs, ss) = 
  match rs with
    [] -> ss
  | r::rs' -> if memberRE (r, ss)
                then unionRE (rs', ss)
              else r::(unionRE (rs', ss))


(*
 * Check if every regexp in the first list is equivalent to a regexp in the
 * second list 
 *
 *  regexp list * regexp list -> bool
 *)

let rec subsetRE (rs, ss) =    
  match rs with
    [] -> true
  | r::rs' -> memberRE (r,ss) && subsetRE (rs',ss)


(* 
 * Compute the empty function of a regular expression
 *
 *  regexp -> regexp
 *)

let rec empty (rexp) =     
  match rexp with
    Zero -> Zero
  | One -> One
  | Char a -> Zero
  | Plus (r1,r2) -> simplify (Plus (empty r1,empty r2))
  | Concat (r1,r2) -> simplify (Concat (empty r1, empty r2))
  | Star _ -> One



(*
 * Compute the derivative of a regular expression
 *
 *  regexp * char -> regexp
 *)

let rec derivative (rexp, a) = 
  match rexp with
    One -> Zero
  | Zero -> Zero
  | Char c -> if (c=a) then One else Zero
  | Plus (r1,r2) -> simplify (Plus (derivative (r1,a), derivative (r2,a)))
  | Concat (r1,r2) -> simplify (Plus (Concat (derivative (r1,a), r2), 
                                      Concat(empty(r1), derivative (r2,a))))
  | Star (r) -> simplify (Concat (derivative (r,a),Star r))



(*
 *  PROVIDE CODE FOR THESE FUNCTIONS FOR QUESTION (3)
 *
 *)

(* Add a regular expression, rexp, to a list, if it isn't already in the list *)
let prependNoRepeat (rexp, lst) = if memberRE(rexp, lst) then lst  else rexp::lst
let rec concatNoRepeat (rexps1, rexps2) = match rexps1 with
  | [] -> rexps2
  | head::tail -> prependNoRepeat(head, concatNoRepeat(tail, rexps2))

let rec allDerivativesSym (l, c) = match l with
  | [] -> []
  | head::tail -> prependNoRepeat(derivative(head, c), allDerivativesSym(tail, c))

let rec allDerivatives (l,  a) = match a with
  | [] -> []
  | head::tail -> concatNoRepeat(allDerivativesSym(l, head), allDerivatives(l, tail))

(* I feel like this is a hacky solution, but I am 
   sleepy, and I'm moving on. *)
let rec makeStatesHelper (rs, old_rs, alphabet) = match rs with
  | [] -> []
  | head::tail when subsetRE(rs, old_rs) -> rs
  | head::tail -> makeStatesHelper(concatNoRepeat(rs, allDerivatives(rs, alphabet)), rs, alphabet)

let makeStates (r, a) = makeStatesHelper([r], [], a)

let rec makeDeltaHelper(state, alphabet) = match alphabet with
  | [] -> []
  | head::tail -> (state, head, derivative(state, head)) :: makeDeltaHelper(state, tail)

let rec makeDelta (states, alphabet) = match states with
  | [] -> []
  | head::tail -> makeDeltaHelper(head, alphabet) @ makeDelta(tail, alphabet)

let rec makeFinalStatesHelper (states, final_states) = match states with
  | [] -> final_states
  | head::tail when empty(head) = One -> makeFinalStatesHelper(tail, head::final_states)
  | head::tail -> makeFinalStatesHelper(tail, final_states)

let makeFinalStates (states) = makeFinalStatesHelper(states, [])

let rec makeDFA (rexp,alphabet) = 
{
  states =   makeStates(rexp, alphabet);
  alphabet = alphabet;
  start =    rexp;
  delta =    makeDelta(makeStates(rexp, alphabet), alphabet);
  final =    makeFinalStates(makeStates(rexp, alphabet))
}



(* Some alphabets *)

let lowercase = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';
		'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']

let uppercase = List.map Char.uppercase lowercase

let letters = lowercase @ uppercase

let numbers = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']

let english = letters@[' '; ','; '.'; ';'; ':'; '\''; '-']


(* Some regular expressions *)

let anyLetter = List.fold_right (fun c r -> Plus (Char c, r)) letters Zero

let tWords = Concat (Plus (Char 't', Char 'T'), Star (anyLetter))


(* 
 * String matching functions
 *
 * NOTE: these functions fail if string contains characters not in 
 *       the provided alphabet
 *)

let matchString (s,regexp,alphabet) = 
  accept (makeDFA (regexp, alphabet), s)

let matchSubstrings (s,regexp,alphabet) =
  let len = String.length s  in
  let d = makeDFA (regexp, alphabet)  in
  let rec matches (start, k) = 
    if k<0 
      then []
    else let ss = String.sub s start k
         in if accept (d,ss)
              then [(ss,start)]
            else matches (start, k-1)  in
  let rec loop (i, acc) = 
    if i<0
      then acc
    else loop (i-1, matches (i, len-i) @ acc)
  in loop (len, [])

