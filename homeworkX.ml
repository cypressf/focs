
(* CODE FOR HOMEWORK X
 *
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



type direction = Left | Right | Stay

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

	
module TM : sig

  type 'a machine

  (* Build Turing machines *)

  val build : ('a -> string) -> 'a tm_desc -> 'a machine
	  
  val build' : string tm_desc -> string machine

  (* Execute Turing machines *)

  val run : 'a machine -> string -> bool

  val run_trace_all : 'a machine -> string -> bool

  val run_trace_some : 'a machine -> 'a list -> string -> bool 

  (* Functions to work with transition functions/tables *)

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
                 | (_,_,Left) -> false | _ -> true)
	(Printf.sprintf "%s moves left" (trans st md.leftmost))  in
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
  let add xs ys = 
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
    | (q',b,Stay) -> C (u,q',b::v')

  let run' f (M (md,ss)) w = 
    let input = List.map Char.escaped (explode w) in
    let _ = checkTrue (List.for_all (fun s -> List.mem s md.input_alph) input)
	        "Input string uses symbols not in input alphabet"  in
    let init_tape = md.leftmost::input  in
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




(* 
 * IMPLEMENT THE FOLLOWING FUNCTIONS
 *
 *)

let repeated () = fail "Function repeated not implemented"

let count () = fail "Function count not implemented"

let free_cell () = fail "Function free_cell not implemented"

let move_right s1 s2 = fail "Function move_right not implemented"
