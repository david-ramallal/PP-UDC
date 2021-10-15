let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1

let rec orbit = function
    1 -> Printf.printf "1\n"
  | n -> Printf.printf "%d, " n ; orbit (f n)

let rec length = function
	1 -> 0
	| n -> 1 + length (f n)
	
let rec top = function
	1 -> 1
	| n -> max n (top (f n))
		
(*	
let rec top = function
	1 -> 1
	|n -> let x = top (f n) in if n > x then n else x
*)

let rec length'n'top = function
	1 -> (0,1)
	| n -> let x = (length'n'top (f n)) in (1 + fst x, if n > snd x then n else snd x)

