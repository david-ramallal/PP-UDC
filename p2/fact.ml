let rec fact = function
0 -> 1
| n -> n * fact (n - 1);;
let arraySize = Array.length Sys.argv;;
if ((arraySize = 2) && (int_of_string Sys.argv.(1) >= 0)) 
then print_endline(string_of_int(fact (int_of_string Sys.argv.(1))))
else print_endline("fact: argumento invalido");;

