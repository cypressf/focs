
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
 * including installing pretty printers in the OCaml shell 
 * for some types of streams
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

let pp_int_stream ppf s = 
  Format.fprintf ppf "%s" (string_of_stream string_of_int 10 s)

let pp_float_stream ppf s = 
  Format.fprintf ppf "%s" (string_of_stream string_of_float 10 s)

let pp_string_stream ppf s = 
  Format.fprintf ppf "%s" (string_of_stream string_of_string 10 s)

let pp_int_int_stream ppf s = 
  Format.fprintf ppf "%s" (string_of_stream string_of_int_int 10 s)

let pp_int_string_stream ppf s = 
  Format.fprintf ppf "%s" (string_of_stream string_of_int_string 10 s)

let pp_int_stream_stream ppf s = 
  Format.fprintf ppf "%s" 
     (string_of_stream (fun x -> (string_of_stream string_of_int 4 x)) 10 s)

let pp_int_int_stream_stream ppf s = 
  Format.fprintf ppf "%s" 
     (string_of_stream (fun x -> (string_of_stream string_of_int_int 4 x)) 
                           10 s)

let pp_int_int_list_stream ppf s = 
  Format.fprintf ppf "%s"
     (string_of_stream (string_of_list string_of_int_int) 10 s)

let pp_int_list_stream ppf s = 
  Format.fprintf ppf "%s"
     (string_of_stream (string_of_list string_of_int) 10 s)


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
 * from k : returns the stream of integers starting at 'k'
 * 
 *)

let rec const k = fby k (fun () -> const k)

let rec from k = fby k (fun () -> from (k + 1))

let nats = from 0

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

let scale n s = fail "Function scale not implemented"

let zip s1 s2 = fail "Function zip not implemented"

let add s1 s2 = fail "Function add not implemented"

let psums s = fail "Function psums not implemented"

let periodic l = fail "Function periodic not implemented"

let running_max s = fail "Function running_max not implemented"



(*
 * QUESTION 2
 * 
 *)

let arctan z = fail "Function arctan not implemented"

let pi () = fail "Function pi not implemented"

let newton f df guess = fail "Function newton not implemented"

let derivative f x = fail "Function derivative not implemented"

let limit mx eps s = fail "Function limit not implemented"



(* 
 * QUESTION 3 
 * 
 *)


(* Coming soon *)
