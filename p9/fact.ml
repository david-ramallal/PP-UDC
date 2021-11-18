let rec fact = function
0 -> 1
| n -> n * fact (n - 1);;
let arraySize = Array.length Sys.argv;;
if (arraySize = 2) then
	try print_endline(string_of_int(fact (int_of_string Sys.argv.(1)))) with
		Stack_overflow -> print_endline("fact: argumento invalido")
		| Failure _ -> print_endline("fact: argumento invalido")
else print_endline("fact: numero de argumentos invalido");;

