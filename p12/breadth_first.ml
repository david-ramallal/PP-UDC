let rec breadth_first = function
	Gt (x, []) -> [x]
	| Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;
;;
let breadth_first_t l = 
	let rec aux acc = function
		Gt (x, []) -> List.rev (x::acc)
		| Gt (x, (Gt (y, t2))::t1) -> aux (x::acc) (Gt (y, List.rev_append (List.rev t1) t2))
	in aux [] l
;;
let t = Gt(0, List.init 300_000 (fun x -> Gt(x, [])));;
