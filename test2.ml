let square x = x * x;;

let rec fact x =
	if x <= 1 then 1 else x * fact (x - 1);;

let rec sort = function
	| [] -> []
	| x :: l -> insert x (sort l)
and insert elem = function
	| [] -> [elem]
	| x :: l -> if elem < x then elem :: x :: l
		else x :: insert elem l;;