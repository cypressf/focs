
(* CODE FOR HOMEWORK 9
 *
 * Streams
 *
 *)


let fail s = raise (Failure s)


  
(* The underlying implementation for streams 
 *
 * Basically, a stream is a pair of an element and a 
 * "promise" to compute the rest of the stream.
 * 
 * That "promise" is represented as a function
 *
 * The implementation memoizes that function: once the function is
 * called once, it remembers its result, and subsequent calls to the
 * function directly return the result without executing the body of
 * the function
 *
 *)
  
module AbsStream : 
    sig
      type 'a stream 
      val mk : 'a -> (unit -> 'a stream) -> 'a stream 
      val unmk1 : 'a stream -> 'a 
      val unmk2 : 'a stream -> 'a stream 
    end = 
  struct
    
    type 'a stream = R of 'a * (unit -> 'a stream)
	  
    let memoize f = 
      let memoized = ref None in
      let new_f () = 
	match !memoized with
	| None -> let result = f () in memoized := Some result; result
	| Some v -> v   in
      new_f
	
    let mk h t = R (h, memoize t) 
    let unmk1 s = let R (h,t) = s in h
    let unmk2 s = let R (h,t) = s in t ()
  end


(*
 * These are the stream functions you will want to use
 *
 *)

type 'a stream = 'a AbsStream.stream
let head : 'a stream -> 'a = AbsStream.unmk1
let tail : 'a stream -> 'a stream = AbsStream.unmk2
let fby : 'a -> (unit -> 'a stream) -> 'a stream = AbsStream.mk





(* 
 * Helper functions to print streams 
 *
 *)

let string_of_stream string_of_a n s = 
  let rec string_of_stream n s = 
    if n <= 0 then "..."
    else (string_of_a (head s))^"; "^(string_of_stream (n - 1) (tail s))  in
  Printf.sprintf "< %s >" (string_of_stream n s)

let string_of_string s = "\""^s^"\""

let string_of_int_int (a,b) = 
  ("(")^(string_of_int a)^(",")^(string_of_int b)^(")")

let string_of_int_string (a,b) = 
  ("(")^(string_of_int a)^(",")^(string_of_string b)^(")")

let string_of_int_stream s = 
  (string_of_stream string_of_int 4 s)

let string_of_list string_of_a xs =
  let rec loop xs = 
    match xs with
    | [] -> "]"
    | [x] -> (string_of_a x)^"]"
    | x::xs' -> (string_of_a x)^";"^(loop xs')  in
  "["^(loop xs)



(*
 * OCaml shell pretty printers for several types of streams
 *
 *)

let pp_stream pp_element n ppf s = 
  let _ = Format.pp_open_box ppf 3  in
  let _ = Format.pp_print_string ppf "<"  in
  let _ = Format.pp_print_space ppf ()  in
  let rec loop s n = 
    if n < 0 then ()
    else begin
      pp_element ppf (head s);
      Format.pp_print_string ppf ";";
      Format.pp_print_space ppf ();
      loop (tail s) (n - 1)
    end  in
  let _ = loop s n  in
  let _ = Format.pp_print_string ppf "..."  in
  let _ = Format.pp_print_space ppf ()  in
  let _ = Format.pp_print_string ppf ">"  in
  Format.pp_close_box ppf ()
  
let pp_string string_of_a ppf a = 
  Format.pp_print_string ppf (string_of_a a)

let pp_list pp_element ppf a = 
  let _ = Format.pp_open_box ppf 3  in
  let _ = Format.pp_print_string ppf "["  in
  let rec loop a = 
    match a with 
    | [] -> Format.pp_print_string ppf "]"
    | [x] -> (pp_element ppf x ; Format.pp_print_string ppf "]")
    | x::xs -> (pp_element ppf x ; 
		Format.pp_print_string ppf ";";
		Format.pp_print_space ppf ();
		loop xs)  in
  let _ = loop a  in
  Format.pp_close_box ppf ()

let pp_int_stream = 
  pp_stream (pp_string string_of_int) 10

let pp_float_stream = 
  pp_stream (pp_string string_of_float) 10

let pp_string_stream = 
  pp_stream (pp_string string_of_string) 10

let pp_int_int_stream = 
  pp_stream (pp_string string_of_int_int) 10

let pp_int_string_stream = 
  pp_stream (pp_string string_of_int_string) 10

let pp_int_stream_stream = 
  pp_stream (pp_stream (pp_string string_of_int) 4) 10

let pp_int_int_stream_stream = 
  pp_stream (pp_stream (pp_string string_of_int_int) 4) 10

let pp_int_int_list_stream = 
  pp_stream (pp_list (pp_string string_of_int_int)) 10

let pp_int_list_stream = 
  pp_stream (pp_list (pp_string string_of_int)) 10


;; #install_printer pp_int_stream 
;; #install_printer pp_float_stream 
;; #install_printer pp_int_int_stream
;; #install_printer pp_int_string_stream
;; #install_printer pp_int_stream_stream
;; #install_printer pp_int_int_stream_stream 
;; #install_printer pp_int_int_list_stream
;; #install_printer pp_int_list_stream
;; #install_printer pp_string_stream




(* 
 * print_stream str n s
 * show str n s
 * list n s 
 *
 * prints the first 'n' elements of stream 's'
 * 'str' is a function to convert the elements of the stream to a 
 * string for printing
 *
 * print_stream : prints the stream as <a1;a2;a3;a4;...>
 * show : prints the stream element one per line
 * list : returns the list of the first 'n' elements of 's'
 *
 *)

let print_stream string_of_a n s = 
  Printf.printf "%s\n" (string_of_stream string_of_a n s)

let rec show string_of_a n s = 
  if n <= 0 then ()
  else begin
    Printf.printf "  %s\n" (string_of_a (head s));
    flush_all ();
    show string_of_a (n - 1) (tail s)
  end

let rec list n s = 
  if n <= 0 then []
  else (head s) :: (list (n-1) (tail s))

(*
 * Extract the 'n'th element of stream 's' 
 *
 *)

let rec nth n s = 
  if n <= 0 then head s
  else nth (n - 1) (tail s)


(* 
 * const k : returns the constant stream of 'k's
 * iter init incr : returns the stream start with 'init' and incrementing
 *                    via 'incr'
 * from k : returns the stream of integers starting at 'k'
 * 
 *)

let rec const k = fby k (fun () -> const k)

let rec iter init incr = fby init (fun () -> iter (incr init) incr)

let from k = iter k (fun x -> x + 1)
let fromf k = iter k (fun x -> x +. 1.)

let nats = from 0
let natsf = fromf 0.

(*
 * map f s : returns the stream obtained by applying 'f' 
 *             to every element of 's' 
 * filter p s : returns the stream of elements of 's' for which 'p' is true
 *
 *)

let rec map f s = fby (f (head s)) (fun () -> (map f (tail s)))

let plus1 s = map (fun x -> x + 1) s

let evens = map (fun x -> 2 * x) nats

let odds = plus1 evens

let squares = map (fun x -> x * x) nats

let rec filter p s = 
  if (p (head s)) 
  then fby (head s) (fun () -> filter p (tail s))
  else filter p (tail s)

let even s = filter (fun x -> (x mod 2 = 0)) s



(*
 * The Sieve of Eratosthenes
 *
 *)


let not_divisible_by n k = not (k mod n = 0)

let rec sieve s = 
   fby (head s) 
       (fun () -> sieve (filter (not_divisible_by (head s)) (tail s)))

let primes = sieve (from 2)




(* 
 * QUESTION 1 
 * 
 *)


let scale n = map (fun e -> e * n)
let scalef n = map (fun e -> e *. n)

let rec zip s1 s2 = 
   fby (head s1, head s2) 
       (fun () -> zip (tail s1) (tail s2))

let rec add s1 s2 =
    fby ((head s1) + (head s2))
       (fun () -> add (tail s1) (tail s2))

let rec addf s1 s2 =
    fby ((head s1) +. (head s2))
       (fun () -> addf (tail s1) (tail s2))

let rec psums s = 
    fby (head s)
        (fun () -> map (fun x -> x + (head s)) (psums (tail s)))

let rec psumsf s = 
    fby (head s)
        (fun () -> map (fun x -> x +. (head s)) (psumsf (tail s)))

let rec periodic l = match l with
    | [] -> fail "Function periodic given empty list"
    | head::tail ->
        fby head
        (fun () -> periodic (tail@[head]))

let running_max s =
    let rec running_max_helper max s =
        let Some new_max =
            if max = None || max < Some (head s) then Some (head s)
            else max
        in
        fby (new_max)
            (fun () -> running_max_helper (Some new_max) (tail s))
    in running_max_helper None s



(*
 * QUESTION 2
 * 
 *)
let arctan_sum z n =
   (-1.0 ** n) *. (z**(2.*.n+.1.))/.(2.*.n +.1.)

let arctan z = 
    psumsf (map (arctan_sum z) natsf)

let pi () = 
    let tupes = zip (scalef 16. (arctan (1./.5.))) (scalef 4. (arctan (1./.239.))) in
    map (fun (e1, e2) -> e1 -. e2) tupes

let derivative_step f x n =
    (f (x +. 1.) -. f x) /. (1./.n)

let derivative f x = fail "Function derivative not implemented"

let newton f df guess = fail "Function newton not implemented"

let limit mx eps s = fail "Function limit not implemented"



(* 
 * QUESTION 3 
 * 
 *)

let table s1 s2 = fail "Function table not implemented"

let stripes s = fail "Function stripes not implemented"

let flatten s = fail "Function flatten not implemented"

let pairs s1 s2 = fail "Function pairs not implemented"
