
(*************************************************************
 * CODE FOR HOMEWORK 7
 *
 * Turing Machines (Structured States)
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



type direction = Left | Right

type symbol = string

type 'a tm_desc = { states : 'a list;
		    input_alph : symbol list;
  		    tape_alph : symbol list;
		    leftmost : symbol;
		    blank : symbol;
		    delta : (('a * symbol) -> ('a * symbol * direction));
		    start : 'a;
		    accept : 'a;
		    reject : 'a }
      
	
(*************************************************************
 * The TM Library
 * 
 *)

module TM : sig

  type 'a machine
	
  val build : ('a -> string) -> 'a tm_desc -> 'a machine
	  
  val build' : string tm_desc -> string machine

  val run : 'a machine -> string -> bool

  val run_trace_all : 'a machine -> string -> bool

  val run_trace_some : 'a machine -> 'a list -> string -> bool 

  val machine_trans_table : 'a machine -> 
                              (('a * symbol) * ('a * symbol * direction)) list

  val make_trans_table : 'a list -> symbol list -> 
                           (('a * symbol) -> ('a * symbol * 'c)) -> 
                             (('a * symbol) * ('a * symbol * 'c)) list

  val make_delta : (('a * symbol) * ('a * symbol * 'c)) list -> 
                        (('a * symbol) -> ('a * symbol * 'c))

  val print_machine : Format.formatter -> 'a machine -> unit

end  = struct
	
  type 'a machine = M of 'a tm_desc * ('a -> string)

  type 'a config = C of (symbol list * 'a * symbol list)

  let errorTM s = raise (Failure ("TM ERROR: "^s))

  let checkTrue b s = if b then () else errorTM s

  let check_delta string_of_state md = 
    let trans st sym = 
      Printf.sprintf "delta(%s,%s)" (string_of_state st) sym in
    let tried_delta (st,sym) = 
      try 
	md.delta (st,sym) 
      with _ -> errorTM (Printf.sprintf "%s undefined" (trans st sym)) in
    let checkState st = 
      let checkSym sym = 
	let (q,s,d) = tried_delta (st,sym) in
	let _ = checkTrue (List.mem q md.states) 
	           (Printf.sprintf "%s yields state %s not in states list"
                      (trans st sym) (string_of_state q))  in
	let _ = checkTrue (List.mem sym md.tape_alph)
	           (Printf.sprintf "%s yields symbol %s not in tape alphabet"
		      (trans st sym) s)  in
	()  in
      List.iter checkSym md.tape_alph  in
    let checkLeftmost st = 
      checkTrue (match md.delta(st,md.leftmost) with
                 | (_,_,Right) -> true | _ -> false)
	(Printf.sprintf "%s does not move right" (trans st md.leftmost))  in
    let checkLoop st sym = 
      checkTrue (match md.delta(st,sym) with (st',_,_) -> st=st')
	(Printf.sprintf "%s does not loop" (trans st sym))  in
    let _ = List.iter checkState md.states  in
    let _ = List.iter checkLeftmost md.states in
    let _ = List.iter (checkLoop md.accept) md.tape_alph  in
    let _ = List.iter (checkLoop md.reject) md.tape_alph  in
    ()


  let check_alphabet md = 
    let _ = checkTrue (List.for_all (fun x -> List.mem x md.tape_alph) 
                                       md.input_alph)
                 "input alphabet not included in tape alphabet" in
    let _ = checkTrue (List.mem md.leftmost md.tape_alph)
	         "leftmost symbol not in tape alphabet"  in
    let _ = checkTrue (List.mem md.blank md.tape_alph)
	         "blank symbol not in tape alphabet"  in
    let _ = checkTrue (List.for_all (fun x -> String.length x = 1) 
			                md.input_alph)
	         "input symbols not all of length 1"  in
    let _ = checkTrue (List.for_all (fun x -> not (x="")) md.tape_alph)
	         "tape symbols not nonempty"  in
    ()

  let check_states md = 
    let _ = checkTrue (List.mem md.start md.states)
              "start state not in states list"  in
    let _ = checkTrue (List.mem md.accept md.states)
	      "accept state not in states list"  in
    let _ = checkTrue (List.mem md.reject md.states)
	      "reject state not in states list"  in
    ()

  let build string_of_state md = 
    let _ = check_alphabet md  in
    let _ = check_states md  in
    let _ = check_delta string_of_state md  in
    M (md, string_of_state)


  (* add elements of xs not already present in ys to ys *)
  let rec add xs ys = 
    List.fold_right (fun x r -> if (List.mem x r) then r else x::r) xs ys
      
  let build' = build (fun s -> s)

  let print_config string_of_state (C (u,q,v)) = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " (string_of_state q)  in
    let _ = print_syms v  in
    print_newline ()
	
  let next md (C (u,q,v) as c) = 
    let rec split_last u = 
      match List.rev u with
      |	[] -> fail "Moving Left from leftmost tape position"
      |	x :: xs -> (List.rev xs, x)  in
    let (a,v') = match v with
                 | [] -> (md.blank,[])
		 | a::v' -> (a,v')  in
    match md.delta (q,a) with
    | (q',b,Left) -> let (u',c) = split_last u in C (u',q',c::b::v')
    | (q',b,Right) -> C (u@[b],q',v')

  let run' f (M (md,ss)) w = 
    let init_tape = md.leftmost::(List.map Char.escaped (explode w))  in
    let rec loop (C (u,q,v) as c) = 
      let _ = f ss c in 
      if (q = md.accept) then true
      else if (q = md.reject) then false
      else loop (next md c)  in
    loop (C ([], md.start, init_tape))

  let run m w = run' (fun _ _ -> ()) m w

  let run_trace_all m w = run' print_config m w	       

  let run_trace_some m states w = 
    let select_print_config string_of_state (C (u,q,v) as c) = 
      if (List.mem q states) 
      then print_config string_of_state c
      else ()  in
    run' select_print_config m w

  let make_trans_table states symbols delta = 
    let get_trans st = 
      List.fold_right (fun sym r -> ((st,sym),delta(st,sym))::r) 
	                 symbols []  in
    List.fold_right (fun st r -> (get_trans st)@r) states []

  let machine_trans_table (M (md,_)) = 
    make_trans_table md.states md.tape_alph md.delta 

  let make_delta table x = List.assoc x table

  let print_machine ppf (M (md,_)) = 
    Format.fprintf ppf "<TM over {%s} %d states>" 
      (String.concat "," md.input_alph)
      (List.length md.states)
end

(* Hack to add a printer to the OCaml shell to "see" Turing machines *)
;; #install_printer TM.print_machine



(*************************************************************
 * Some sample Turing machines, from last homework
 *
 *)

let asbs = 
  let delta (q,a) = match q,a with
                    | "start", "a" -> ("start", "a", Right)
		    | "start", "b" -> ("q1", "b", Right)
		    | "start", ">" -> ("start", ">", Right)
		    | "start", "_" -> ("acc", "_", Right)
		    | "q1", "a" -> ("rej", "a", Right)
		    | "q1", "b" -> ("q1", "b", Right)
		    | "q1", ">" -> ("rej", ">", Right)
		    | "q1", "_" -> ("acc", "_", Right)
		    | "acc", sym -> ("acc", sym, Right)
		    | _, sym -> ("rej", sym, Right)  in
  TM.build' { states = ["start"; "q1"; "acc"; "rej"];
	      input_alph = ["a";"b"];
	      tape_alph = ["a"; "b"; "_"; ">"];
	      leftmost = ">";
	      blank = "_";
	      delta;
	      start = "start";
	      accept = "acc";
	      reject = "rej" }


let anbn_desc =
  let delta (q,a) = 
    match q,a with
    | "start", "a" -> ("start", "a", Right)
    | "start", "b" -> ("q1", "b", Right)
    | "start", ">" -> ("start", ">", Right)
    | "start", "_" -> ("q2", "_", Right)
    | "start", "X" -> ("rej", "X", Right)
    | "q1", "b" -> ("q1", "b", Right)
    | "q1", "_" -> ("q2", "_", Right)
    | "q1", sym -> ("rej", sym, Right)
    | "q2", ">" -> ("q3", ">", Right)
    | "q2", sym -> ("q2", sym, Left)
    | "q3", "X" -> ("q3", "X", Right)
    | "q3", "_" -> ("acc", "_", Right)
    | "q3", "a" -> ("q4", "X", Right)
    | "q3", sym -> ("rej", sym, Right)
    | "q4", "a" -> ("q4", "a", Right)
    | "q4", "X" -> ("q4", "X", Right)
    | "q4", "b" -> ("q2", "X", Right)
    | "q4", sym -> ("rej", sym, Right)
    | "acc", sym -> ("acc", sym, Right)
    | _, sym -> ("rej", sym, Right)  in
  { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
    input_alph = ["a";"b"];
    tape_alph = ["a"; "b"; "X"; "_"; ">"];
    leftmost = ">";
    blank = "_";
    delta;
    start = "start";
    accept = "acc";
    reject = "rej" }

let anbn = TM.build' anbn_desc

let anbncn = 
  let delta (q,a) = match q,a with
                    | "start", "a" -> ("start", "a", Right)
		    | "start", "b" -> ("q1", "b", Right)
		    | "start", "c" -> ("q6", "c", Right)
		    | "start", ">" -> ("start", ">", Right)
		    | "start", "_" -> ("q2", "_", Right)
		    | "start", "X" -> ("rej", "X", Right)
		    | "q1", "b" -> ("q1", "b", Right)
		    | "q1", "c" -> ("q6", "c", Right)
		    | "q1", "_" -> ("q2", "_", Right)
		    | "q1", sym -> ("rej", sym, Right)
		    | "q2", ">" -> ("q3", ">", Right)
		    | "q2", sym -> ("q2", sym, Left)
		    | "q3", "X" -> ("q3", "X", Right)
		    | "q3", "_" -> ("acc", "_", Right)
		    | "q3", "a" -> ("q4", "X", Right)
		    | "q3", sym -> ("rej", sym, Right)
		    | "q4", "a" -> ("q4", "a", Right)
		    | "q4", "X" -> ("q4", "X", Right)
		    | "q4", "b" -> ("q5", "X", Right)
		    | "q4", sym -> ("rej", sym, Right)
		    | "q5", "b" -> ("q5", "b", Right)
		    | "q5", "X" -> ("q5", "X", Right)
		    | "q5", "c" -> ("q2", "X", Right)
		    | "q5", sym -> ("rej", sym, Right)
		    | "q6", "c" -> ("q6", "c", Right)
		    | "q6", "_" -> ("q2", "_", Right)
		    | "q6", sym -> ("rej", sym, Right)
		    | "acc", sym -> ("acc", sym, Right)
		    | _, sym -> ("rej", sym, Right)  in
  TM.build' { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	      input_alph = ["a"; "b"; "c"];
	      tape_alph = ["a"; "b"; "c"; "X"; "_"; ">"];
	      leftmost = ">";
	      blank = "_";
	      delta;
	      start = "start";
	      accept = "acc";
	      reject = "rej" }


(* From the second midterm... *)

let powers2 = 
  let delta (q,a) = 
    match q,a with
    | "start", ">" -> ("1", ">", Right)
	  
    | "1", "X" -> ("1", "X", Right)
    | "1", "a" -> ("2", "a", Right)

    | "2", "X" -> ("2", "X", Right)
    | "2", "a" -> ("3", "a", Left)
    | "2", "_" -> ("acc", "_", Right)
	  
    | "3", ">" -> ("4", ">", Right)
    | "3", sym -> ("3", sym, Left)

    | "4", "X" -> ("4", "X", Right)
    | "4", "a" -> ("5", "X", Right)
    | "4", "_" -> ("6", "_", Left)

    | "5", "X" -> ("5", "X", Right)
    | "5", "a" -> ("4", "a", Right)

    | "6", ">" -> ("1", ">", Right)
    | "6", sym -> ("6", sym, Left)

    | "acc", sym -> ("acc", sym, Right)
    | _, sym -> ("rej", sym, Right)  in
  TM.build' { states = ["start";"1";"2";"3";"4";"5";"6";"acc";"rej"];
	      input_alph = ["a"];
	      tape_alph = ["a"; "X"; "_"; ">" ];
	      leftmost = ">";
	      blank = "_";
	      delta;
	      start = "start";
	      accept = "acc";
	      reject = "rej" }


(* 
 * Turing machine to accept strings over {0,1} with:
 * - an even number of 0s and never more than two 0s in sequence; and
 * - an odd number of 0s and never more than two 1s in sequence.
 *
 *) 

let evenOddSequence_desc = 
  let delta (p,a) = 
    match p,a with
    | "even", ">" -> ("even", ">", Right)
    | "even", "0" -> ("odd", "0", Right)
    | "even", "1" -> ("even", "1", Right)
    | "even", "_" -> ("q1/0", "_", Left)
	  
    | "odd", "0" -> ("even", "0", Right)
    | "odd", "1" -> ("odd", "1", Right)
    | "odd", "_" -> ("q1/1", "_", Left)
	  
    | "q1/0", ">" -> ("acc", ">", Right)
    | "q1/0", "0" -> ("q2/0", "0", Left)
    | "q1/0", "1" -> ("q1/0", "1", Left)
	  
    | "q2/0", ">" -> ("acc", ">", Right)
    | "q2/0", "0" -> ("q3/0", "0", Left)
    | "q2/0", "1" -> ("q1/0", "1", Left)
	  
    | "q3/0", ">" -> ("acc", ">", Right)
    | "q3/0", "1" -> ("q1/0", "1", Left)
	  
    | "q1/1", ">" -> ("acc", ">", Right)
    | "q1/1", "0" -> ("q1/1", "0", Left)
    | "q1/1", "1" -> ("q2/1", "1", Left)
	  
    | "q2/1", ">" -> ("acc", ">", Right)
    | "q2/1", "0" -> ("q1/1", "0", Left)
    | "q2/1", "1" -> ("q3/1", "1", Left)
	  
    | "q3/1", ">" -> ("acc", ">", Right)
    | "q3/1", "0" -> ("q1/1", "0", Left)
	  
    | "acc", sym -> ("acc", sym, Right)
    | _, sym -> ("rej", sym, Right)  in
  { states = [ "even";"odd";"q1/0";"q2/0";"q3/0";"q1/1";"q2/1";"q3/1";
	       "acc";"rej"];
    input_alph = ["0";"1"];
    tape_alph = ["0";"1";"_";">"];
    leftmost = ">";
    blank = "_";
    delta;
    start = "even";
    accept = "acc";
    reject = "rej" }

let evenOddSequence = TM.build' evenOddSequence_desc


let evenOddSequence_tagged_desc = 
  let v x = 
    match x with
    | "0" -> 0
    | "1" -> 1
    | _ -> -1  (* a value that never matches *)  in
  let delta (p,a) = 
    match p,a with
    | ("even", None), ">" -> (("even", None), ">", Right)
    | ("even", None), "0" -> (("odd", None), "0", Right)
    | ("even", None), "1" -> (("even", None), "1", Right)
    | ("even", None), "_" -> (("q1", Some 0), "_", Left)
	  
    | ("odd", None), "0" -> (("even", None), "0", Right)
    | ("odd", None), "1" -> (("odd", None), "1", Right)
    | ("odd", None), "_" -> (("q1", Some 1), "_", Left)
	  
    | ("q1", Some t), ">" -> (("acc", None), ">", Right)
    | ("q1", Some t), sym when t = v sym -> (("q2", Some t), sym, Left)
    | ("q1", Some t), sym -> (("q1", Some t), sym, Left)
	  
    | ("q2", Some t), ">" -> (("acc", None), ">", Right)
    | ("q2", Some t), sym when t = v sym -> (("q3", Some t), sym, Left)
    | ("q2", Some t), sym -> (("q1", Some t), sym, Left)
	  
    | ("q3", Some t), ">" -> (("acc", None), ">", Right)
    | ("q3", Some t), sym when t <> v sym -> (("q1", Some t), sym, Left)
	  
    | ("acc", None), sym -> (("acc", None), sym, Right)
    | _, sym -> (("rej", None), sym, Right)  in
  { states = [ ("even", None); ("odd", None); ("acc", None); ("rej", None);
	       ("q1", Some 0); ("q2", Some 0); ("q3", Some 0); 
	       ("q1", Some 1); ("q2", Some 1); ("q3", Some 1) ];
    input_alph = ["0"; "1"];
    tape_alph = ["0"; "1"; ">"; "_"];
    leftmost = ">";
    blank = "_";
    delta ;
    start = ("even", None);
    accept = ("acc", None);
    reject = ("rej", None) }

let evenOddSequence_tagged = 
  let string_of_state st = 
    match st with
    | (s,None) -> s
    | (s,Some b) -> s^"|"^(string_of_int b)  in
  TM.build string_of_state evenOddSequence_tagged_desc





(*************************************************************
 * PROBLEM 2
 * 
 *)


let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let intDigits = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
let isDigit c = List.mem c digits

let crossNone xs = 
  List.map (fun x -> (x,None)) xs

let cross2 xs ys =   
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []

let crossSome xs ys = 
  cross2 xs (List.map (fun y -> Some y) ys)

let crossSomeNone xs ys =
  List.fold_right (fun x r -> (List.map (fun y -> (x, Some y, None)) ys)@r) xs []

let crossNoneNone = List.map (fun x -> (x, None, None))

let crossSomeSome xs ys zs =
  List.fold_right 
  (fun x r -> (List.fold_right 
                 (fun y r -> (List.map (fun z -> (x, Some y, Some z)) zs)@r) ys [])@r)
     xs []

let cross3 xs ys zs =   
  List.fold_right 
    (fun x r -> (List.fold_right 
                   (fun y r -> (List.map (fun z -> (x,y,z)) zs)@r) ys [])@r)
       xs []


let string_of_state_sym_opt st = 
  match st with
  | (s,None) -> s
  | (s,Some sym) -> s^"|"^sym 

let string_of_state_int_int_opt st = 
  match st with
  | (s,None) -> s
  | (s,Some (c,d)) -> s^"|"^(string_of_int c)^"|"^(string_of_int d)



(* IMPLEMENT THE FOLLOWING FUNCTIONS FOR PROBLEM 2 *)

(*
Return a TM that decides the language {u#u | u is in digits}
The TM must use structured states (so much easier)
*)
let same_pair () =
  let delta (p,a) = match p, a with
    | ("start", None), ">" -> ("start", None), ">", Right
    | ("start", None), digit when isDigit digit -> ("check first number", None), digit, Right
        
    | ("check first number", None), digit when isDigit digit -> ("check first number", None), digit, Right
    | ("check first number", None), "#" -> ("check second number", None), "#", Right

    | ("check second number", None), digit when isDigit digit -> ("check second number", None), digit, Right
    | ("check second number", None), "_" -> ("x first digit", None), "_", Left

    | ("x first digit", None), digit when isDigit digit -> ("skip to #", Some digit), "x", Left
    | ("x first digit", None), "#" -> ("more digits?", None), "#", Left

    | ("more digits?", None), "x" -> ("more digits?", None), "x", Left
    | ("more digits?", None), ">" -> ("acc", None), ">", Right

    | ("skip to #", Some digit), d when isDigit d -> ("skip to #", Some digit), d, Left
    | ("skip to #", Some digit), "#" -> ("find digit", Some digit), "#", Left

    | ("find digit", Some digit), "x" -> ("find digit", Some digit), "x", Left
    | ("find digit", Some digit), d when d = digit -> ("rewind 1", None), "x", Right

    | ("rewind 1", None), "x" -> ("rewind 1", None), "x", Right
    | ("rewind 1", None), "#" -> ("rewind 2", None), "#", Right
    | ("rewind 2", None), d when isDigit d -> ("rewind 2", None), d, Right
    | ("rewind 2", None), "x" -> ("x first digit", None), "x", Left

    | ("acc", None), sym -> (("acc", None), sym, Right)
    | _, sym -> ("rej", None), sym, Right
  in
  TM.build string_of_state_sym_opt 
  {
    states = crossNone [
      "start";
      "check first number";
      "check second number";
      "x first digit";
      "more digits?";
      "rewind 1";
      "rewind 2";
      "acc";
      "rej"
    ] @
    crossSome [
      "skip to #";
      "find digit"
    ] digits;
    input_alph = digits;
    tape_alph = digits @ ["_"; ">"; "x"];
    leftmost = ">";
    blank = "_";
    delta;
    start = ("start", None);
    accept = ("acc", None);
    reject = ("rej", None)
  }

let get_new_state carry digit =
  if carry = 0 then
    "skip to #", Some (0, digit)
  else
    if digit = 9 then
      "skip to #", Some (1, 0)
    else
      "skip to #", Some (0, digit + 1)


let pred_pair () =
  let delta (p,a) = match p, a with
    | ("start", None), ">" -> ("start", None), ">", Right
    | ("start", None), digit when isDigit digit -> ("check first number", None), digit, Right
        
    | ("check first number", None), digit when isDigit digit -> ("check first number", None), digit, Right
    | ("check first number", None), "#" -> ("check second number", None), "#", Right

    | ("check second number", None), digit when isDigit digit -> ("check second number", None), digit, Right
    | ("check second number", None), "_" -> ("x first digit", Some (1, 0)), "_", Left

    | ("x first digit", Some (carry, _)), digit when isDigit digit -> (get_new_state carry (int_of_string digit)), "x", Left
    | ("x first digit", Some (carry, _)), "#" when carry = 0 -> ("more digits?", None), "#", Left

    | ("more digits?", None), "x" -> ("more digits?", None), "x", Left
    | ("more digits?", None), ">" -> ("acc", None), ">", Right

    | ("skip to #", Some (carry, digit)), d when isDigit d -> ("skip to #", Some (carry, digit)), d, Left
    | ("skip to #", Some (carry, digit)), "#" -> ("find digit", Some (carry, digit)), "#", Left

    | ("find digit", Some (carry, digit)), "x" -> ("find digit", Some (carry, digit)), "x", Left
    | ("find digit", Some (carry, digit)), d when d = string_of_int digit -> ("rewind 1", Some (carry, 0)), "x", Right

    | ("rewind 1", Some (carry, _)), "x" -> ("rewind 1", Some (carry, 0)), "x", Right
    | ("rewind 1", Some (carry, _)), "#" -> ("rewind 2", Some (carry, 0)), "#", Right
    | ("rewind 2", Some (carry, _)), d when isDigit d -> ("rewind 2", Some (carry, 0)), d, Right
    | ("rewind 2", Some (carry, _)), "x" -> ("x first digit", Some (carry, 0)), "x", Left

    | ("acc", None), sym -> (("acc", None), sym, Right)
    | _, sym -> ("rej", None), sym, Right
  in
  TM.build string_of_state_int_int_opt 
  {
    states = crossNone [
      "start";
      "check first number";
      "check second number";
      "more digits?";
      "acc";
      "rej"
    ] @
    crossSome [
      "x first digit";
      "rewind 1";
      "rewind 2";
      "skip to #";
      "find digit";
    ] (cross2 intDigits intDigits);
    input_alph = digits;
    tape_alph = digits @ ["_"; ">"; "x"];
    leftmost = ">";
    blank = "_";
    delta;
    start = ("start", None);
    accept = ("acc", None);
    reject = ("rej", None)
  }



(*************************************************************
 * PROBLEM 3 
 * 
 *)


type direction_S = L | R | S

type 'a tm_desc_S = { states_S : 'a list;
		      input_alph_S : symbol list;
  		      tape_alph_S : symbol list;
		      leftmost_S : symbol;
		      blank_S : symbol;
		      delta_S : (('a * symbol) -> ('a * symbol * direction_S));
		      start_S : 'a;
		      accept_S : 'a;
		      reject_S : 'a }


(* IMPLEMENT THE FOLLOWING FUNCTIONS FOR PROBLEM 3 *)

let transf_states m = 
  crossNone m.states_S @ crossSome m.states_S m.states_S

let transf_delta m (st,sym) = 
  let (state, tag) = st in
  if tag = None then
    let (new_state, new_sym, direction) = m.delta_S (state, sym) in
    if direction = S then
      ((new_state, Some new_state), new_sym, Right)
    else if direction = L then
      ((new_state, None), new_sym, Left)
    else
      ((new_state, None), new_sym, Right)
  else if sym <> m.leftmost_S then
    let Some new_state = tag in
    ((new_state, None), sym, Left)
  else
    ((m.reject_S, None), sym, Right)


let build_S string_of_state m = 
  let d = transf_delta m in
  let machine =
  { states = transf_states m;
    input_alph = m.input_alph_S;
    tape_alph = m.tape_alph_S;
    leftmost = m.leftmost_S;
    blank = m.blank_S;
    delta = d;
    start = (m.start_S, None);
    accept = (m.accept_S, None);
    reject = (m.reject_S, None) } in
  TM.build string_of_state machine


      

let test_asbs_desc = 					 
  let delta (q,a) = 
    match q,a with
    | "start", "a" -> ("start", "a", R)
    | "start", "b" -> ("q0", "b", S)
    | "start", ">" -> ("start", ">", R)
    | "start", "_" -> ("acc", "_", R)
    | "q0", "b" -> ("q1", "b", S)
    | "q1", "a" -> ("rej", "a", S)
    | "q1", "b" -> ("q1", "b", R)
    | "q1", ">" -> ("rej", ">", S)
    | "q1", "_" -> ("acc", "_", S)
    | "acc", sym -> ("acc", sym, R)
    | _, sym -> ("rej", sym, R)  in
  { states_S = ["start"; "q0"; "q1"; "acc"; "rej"];
    input_alph_S = ["a";"b"];
    tape_alph_S = ["a";"b";">";"_"];
    leftmost_S = ">";
    blank_S = "_";
    delta_S = delta;
    start_S = "start";
    accept_S = "acc";
    reject_S = "rej" }
