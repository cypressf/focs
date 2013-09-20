type number = I of int | F of float;;
I 5;;
I 10;;
F 30.0;;

let add_num m n =
  match (m, n) with
    (I i, I j) -> I (i + j)
  | (I i, F f) -> F (float i +. f)
  | (F f, I i) -> F (f +. float i)
  | (F f, F g) -> F (f +. g);;

(* List of pairs, all first components of each 2-tuple *)
let rec project = function
    [] -> []
  | (a,_)::xs -> a::(project xs);;

let rec list_length = function
    [] -> 0
  | _::xs -> 1 + list_length xs;;

let rec sum_list = function
    [] -> 0
  | x::xs -> x + (sum_list xs);;

let rec sumn n = if (n<=0) then 0 else n + sumn (n - 1);;

let rec sumn_for n =
  let answer = ref 0
  in
    for i = 0 to n do
      answer := !answer + i
    done;
  !answer;;

let rec sumn x = match x with
    0 -> x
  | _ -> x + (sumn (x - 1));;