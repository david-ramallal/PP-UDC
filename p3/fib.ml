let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2)

let printFib n = (print_endline(string_of_int(fib n)))

let rec fib_to n =
if n >= 0 then (fib_to (n-1); printFib n)
else ()

let arraySize = Array.length Sys.argv;;
if ((arraySize = 2) && (int_of_string Sys.argv.(1) >= 0)) 
then fib_to (int_of_string Sys.argv.(1))
else print_endline("fib: argumento invalido");;

