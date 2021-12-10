open List;;


let dentro_tablero m n (x,y) = 
	x >= 1 &&  x <= m && y >= 1 && y <= n
;;
let movimientos m n (x,y) l =
	let posibles = [(x-2, y+1); (x-1, y+2); (x+1, y+2); (x+2, y+1); (x-2, y-1); (x+2, y-1); (x-1, y-2); (x+1, y-2)]
	in filter (fun p -> not (mem p l)) (filter (dentro_tablero m n) posibles) 
;;
let shortest_tour m n ini fin = 
	if not ((dentro_tablero m n ini) && (dentro_tablero m n fin))
	then raise (Invalid_argument "tour")
	else 
		let rec aux (h::t) next = 
			if h = fin then rev (h::t)
					   else match next with 
							[] -> raise Not_found
							| hn::tn -> try aux (hn::h::t) (movimientos m n hn (hn::h::t))
									    with
											Not_found -> aux (h::t) tn	
		in aux [ini] (movimientos m n ini [])
;;
