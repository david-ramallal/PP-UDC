let rec remove n l = match l with
	[] -> []
	| h::t -> if n = h then t else h::remove n t

let rec remove_all n l = match l with
	[] -> []
	| h::t -> if n = h then remove n t else h::remove n t
	
let rec lprod l1 l2 = match l1, l2 with
	[],[] -> []
	|(h1::t1),(h2::t2) -> (h1,t2) :: lprod l1 t2
	| 
	

	
