(*
let rec to0from n =
	if n < 0 then []
	else n :: to0from (n-1);;
*)
let to0from n = 
	let rec aux i l = 
		if i > n then l 
		else aux (i+1) (i::l)
	in 
		aux 0 []
(*	
let rec fromto m n =
	if m > n then []
	else m :: fromto (m+1) n;;
*)
let fromto m n = 
	let rec aux i l =
		if i < m then l
		else aux (i-1) (i::l)
	in
		aux n []
(*	
let rec from1to n =
	if n < 1 then []
	else from1to (n-1) @ [n];;
*)
let from1to n = fromto 1 n
(*
let map =
	List.map;;
*)
let map f l= List.rev (List.rev_map f l )
(*	
let power x y =
	let rec innerpower x y =
		if y = 0 then 1
		else x * innerpower x (y-1)
	in
		if y >= 0 then innerpower x y
		else invalid_arg "power";;
*)
let power x y =
	let rec aux acc i =
		if i < 1 then acc
		else aux (acc*x) (i-1)
	in
		if y >= 0 then aux 1 y
		else invalid_arg "power"
(*		
let incseg l =
	List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;
*)
let incseg l = 
  let rec aux l1 l2 acc = match l1 with
    [] -> []
    | [h] -> List.rev ((acc + h)::l2)
    | h::t -> aux t ((acc + h)::l2) (acc + h)
  in aux l [] 0
(*	
let rec remove x = function
	[] -> []
	| h::t -> if x = h then t
			  else h :: remove x t;;
*)
let remove x l =
	let rec aux acc = function
		[] -> l
		|h::t -> if x = h then List.rev_append acc t
				else aux (h::acc) t 
  in aux [] l 	
(*
let rec divide = function 
	h1::h2::t -> let l1, l2 = divide t in h1::l1, h2::l2
	| l -> l, []
Para hacer divide terminal usar 2 acumuladores (no hace falta darle la vuelta, orden no importa)
*)
let divide l =
	let rec aux l1 l2 = function
		[] -> [],[]
		| h::t -> if h mod 2 <> 0 then 
	in aux [] []

(*		  
let rec compress = function
	| h1::h2::t -> if h1 = h2 then compress (h2::t)
				  else h1 :: compress (h2::t)
	| l -> l;;
*)
