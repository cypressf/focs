let rec fold_right base_case f l = match l with
	[] -> base_case
	| x :: xs -> f x (fold_right base_case f xs)

let genMap = fold_right []

let sum = fold_right 0 (+)

let last = fold_right None (fun x y -> if y = None then x else y)
let removeNone = fold_right [] (fun x ys -> if x = None then ys else x::ys) 
(* let removeOption = *)

let rec last = function 
	| [] -> None
	| head :: tail -> if List.length tail = 0 then Some head else last tail