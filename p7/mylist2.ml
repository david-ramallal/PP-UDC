let hd = function
	[] -> raise(Failure "hd")
	| h::_ -> h

let tl = function
	[] -> raise(Failure "tl")
	| _::t -> t
	
let length l =
	let rec aux i = function
		[] -> i
		| _::t -> aux (i+1) t				
	in
		aux 0 l
	
let rec compare_lengths = function 
	[] -> (function [] -> 0
					| _-> (-1))
	| _::t -> (function [] -> 1
					| _::t2 -> compare_lengths t t2) 

let rec nth l n = 
	if n < 0 then raise(Invalid_argument "nth")
	else let aux = function
		[] -> raise(Failure "nth")
		| h::t -> (function 	
						0 -> h
						| n -> nth t (n-1))
	in aux l n
	
let rec append  = function  
	[] -> (function l -> l)
	| h::t -> (function l -> h :: append t l)

let rec find f l = match l with
	[] -> raise(Not_found)
	| h::t -> if f h then h else find f t
	
let rec for_all p = function
	[] -> true											
	| h::t ->  p h && for_all p t
	
let rec exists f l = match l with
	[] -> false
	| h::t -> if f h then true else exists f t
	
let rec mem x = function
	[] -> false
	| h::t -> x = h || mem x t
	
let rec rev_append l1 l2 = match l1 with
	[] -> l2
	| h::t -> rev_append t (h::l2)

let rev l =
	rev_append l []  
	
let rec filter f l =
  let rec aux f l aux2 = match l with
    [] -> aux2
    | h::t -> if f h then aux f t (h::aux2)
			  else aux f t aux2
    in aux f (rev l) [];; 
	
let find_all = filter
	
let rec aux_partition f l = match l with
	[] -> []
	| h::t -> if f h then aux_partition f t else h::(aux_partition f t) 
	
let partition f l = match l with
	[] -> [],[]
	| h::t -> (filter f l, aux_partition f l)

let rec split l = match l with
	[] -> [],[]
	|(h1, h2)::t -> let (l1, l2) = split t in (h1::l1, h2::l2)
	
let rec combine l1 l2 = match (l1, l2) with
	[],[] -> []
	|(h1::t1),(h2::t2) -> (h1, h2)::combine t1 t2
	|(_,_) -> raise(Invalid_argument"combine")                  
	
let init n f =
  if n < 0
    then raise (Invalid_argument"len < 0")
    else let rec aux (i, l) =
      if i = n then l else aux(i + 1, f i::l)
    in rev(aux(0, []))

let rec concat l = match l with
	[] -> []
	| h::t -> append h (concat t)

let flatten = concat

let rec map f = function
	[] -> []
	| h::t -> f h :: map f t

let rev_map f l =
  let rec aux l auxl = match l with
    [] -> auxl
    | h::t -> aux t (f(h)::auxl)
  in aux l [];;

let rec map2 f l1 l2 =
  if (length l1 <> length l2)
    then raise (Invalid_argument "map2") else if (length l1 == 0)
    then [] else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2)

let rec fold_left f a = function
  [] -> a
  | h::t -> fold_left f (f a h) t

let rec fold_right f l b = match l with
  [] -> b
  | h::t -> f h (fold_right f t b)





