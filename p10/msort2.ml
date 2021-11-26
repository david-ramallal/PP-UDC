let rec divide l = match l with
	h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
	| _ -> l, [];;
(*
let rec merge = function
	[], l | l, [] -> l
	| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
					   else h2 :: merge (h1::t1, t2);;

let rec msort1 l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = divide l in
			merge (msort1 l1, msort1 l2);;
*)
let rec merge ord = function
	[], l | l, [] -> l
	| h1::t1, h2::t2 -> if ord h1 h2 then h1 :: merge ord (t1, h2::t2)
					   else h2 :: merge ord (h1::t1, t2);;

let rec msort1 ord l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = divide l in
			merge ord (msort1 ord l1, msort1 ord l2);;
			
(*
La no terminalidad de divide y de merge puede llegar a provocar Stack Overflow
en casos de listas grandes. Por ejemplo con la lista:
let l2 = List.init 150_000 (fun _ -> Random.int 1000000);;
msort1 (<) l2 produce Stack Overflow
*)

let divide' l = 
	let rec aux acc1 acc2 = function
		h1::h2::t -> aux (h1::acc1) (h2::acc2) t
		| [] -> (List.rev acc1, List.rev acc2)
		| h::[] -> (List.rev (h::acc1), List.rev acc2)
	in aux [] [] l

let merge' ord (l1, l2) = 
	let rec aux acc (r1, r2)= match r1, r2 with
		[], l | l, [] -> List.rev_append acc l
		| h1::t1, h2::t2 -> if ord h1 h2 then aux (h1::acc) (t1, h2::t2)
						   else aux (h2::acc) (h1::t1, t2)
	in aux [] (l1, l2)

let rec msort2 ord l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = divide' l in
			merge' ord (msort2 ord l1, msort2 ord l2);;

(*
Comparación de tiempos entre qsort2, msort1 y msort2:
let ejemplo1 = List.init 150_000 (fun _ -> Random.int 1000000);;
	qsort2 (<) ejemplo1
		0.423847999999997782
	msort1 (<) ejemplo1
		Stack Overflow
	msort2 (<) ejemplo1
		0.458818000000000836
		
let ejemplo2 = List.init 30_000 (fun _ -> Random.int 1000000);;
	qsort2 (<) ejemplo2
		0.0926949999999990837
	msort1 (<) ejemplo2
		float = 0.0657589999999999
	msort2 (<) ejemplo2
		float = 0.0673720000000006536

*)
