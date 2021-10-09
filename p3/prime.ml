let is_prime n =
let rec check_from i =
i >= n ||
(n mod i <> 0 && check_from (i+1))
in check_from 2;;

let rec next_prime n =
if is_prime (n+1) then n+1
else next_prime (n+1);;

let rec last_prime_to n =
if is_prime n then n
else last_prime_to (n-1);;

let is_prime2 n =
let rec check_from i =
i * i > n ||
(n mod i <> 0 && check_from (i+1))
in n > 1 && check_from 2;;

(*
is_prime 1_000_000_007 tarda 23.827197s
is_prime2 1_000_000_007 tarda 0.004539s

Medido con:
let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx
*)
