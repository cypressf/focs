(*
Cypress Frankenfeld
iamcypress@gmail.com
Sorry for not finishing. I'll submit more as soon as possible.
*)

(* part 1 *)
let rec append (list1, list2) =
	let list_reverse = List.rev(list1) in
	match (list_reverse, list2) with
	(_, []) -> list1
	| ([], _) -> list2
	| (head::tail, _) -> let tail_forward = List.rev(tail) in append(tail_forward, head :: list2);;

let rec flatten (lst) = match lst with
	[] -> []
	| [[]] -> []
	| first_list :: second_list :: other_lists -> flatten( append(first_list, second_list) :: other_lists )
	| [head::tail] -> head::tail;;

let rec double (lst) = match lst with
	[] -> []
	| head::tail -> 2 * head :: double(tail);;

(* I feel dirty using List.nth. Is there some other way you wanted us to do this? *)
let last (list1) = match list1 with
	[] -> None
	| _::_ -> Some (List.nth list1 ((List.length list1) - 1));;

(* part 2 *)

type rat = {num: int; den: int}

let floatR (r) =
	float(r.num) /. float(r.den)

let rec gcd (a, b) =
	if a = 0 then b
	else if b = 0 then a
	else if a > b then gcd (b, (a mod b))
	else if b > a then gcd (a, (b mod a))
	else a;;

let simplify (r) = 
	let divisor = gcd (r.num, r.den) in
	{num = (r.num / divisor); den = (r.den / divisor)};;

let addR (r1, r2) =
	simplify( {num = r1.num * r2.den + r2.num * r1.den; den =  r1.den * r2.den});;

let multR (r1, r2) =
	simplify( {num = r1.num * r2.num; den = r1.den * r2.den});;

type number =
	I of int
	| R of rat
	| F of float


let add (n1, n2) = match (n1, n2) with
	(I i1, I i2) -> I (i1 + i2) 
	| (I i1, R r2) -> R (addR({num = i1; den = 1}, r2) )
	| (I i1, F f2) -> F (float(i1) +. f2)
	| (R r1, F f2) -> F (floatR(r1) +. f2)
	| (R r1, I i2) -> R (addR(r1, {num = i2; den = 1}))
	| (R r1, R r2) -> R (addR(r1, r2))
	| (F f1, R r2) -> F (f1 +. floatR(r2))
	| (F f1, F f2) -> F (f1 +. f2)
	| (F f1, I i2) -> F (f1 +. float(i2));;

(* part 5 *)
type bConst = One | Zero
type bExpr =
	Const of bConst
	| Var of string
	| And of bExpr * bExpr
	| Or of bExpr * bExpr
	| Not of bExpr


(* let rec vars (expr, var_list) =
	if expr = Const
		[]
	else if expr = Var
		[v]
	else if expr = And
	Const c -> []
	| Var v -> [v]
	| And (a, b) -> vars (a) @ vars (b)
	| Or (a, b) -> vars (a) :: vars (b)
	| Not a -> vars (a)
	| Var 

 *)

