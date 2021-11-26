let rec qsort1 ord = function
[] -> []
| h::t -> let after, before = List.partition (ord h) t in
qsort1 ord before @ h :: qsort1 ord after;;

(*
La implementación de Quicksort qsort1 tiene varios problemas. Por un lado el uso de @ provoca  
que no sea terminal. Por otro, el algoritmo es altamente ineficiente ya que toma como pivote
el primer elemento, por lo que si la lista ya está ordenada inicialmente dará problemas
*)

let rec qsort2 ord =
let append' l1 l2 = List.rev_append (List.rev l1) l2 in
function
[] -> []
| h::t -> let after, before = List.partition (ord h) t in
append' (qsort2 ord before) (h :: qsort2 ord after);;

(*
Al utilizar append' en lugar del @, qsort2 ofrece la ventaja de la terminalidad

Al hacer "qsort1 (<) l1" se produce Stack Overflow, mientras que "qsort2 (<) l1"
sí que funciona correctamente
*)
let l1 = List.init 300_000 (fun _ -> Random.int 1000000);;

(*
qsort2 tiene la desventaja respecto a qsort1 de que es mas lento debido a que
hay que darle la vuelta a la lista (con List.rev_append)
Tiempos con :
	let l2 = List.init 150_000 (fun _ -> Random.int 1000000);;
		# crono (qsort1 (<)) l2;;                                   
		- : float = 0.424636999999999709
		# crono (qsort2 (<)) l2;;                                   
		- : float = 0.449923999999999324
	qsort2 es casi un 6% mas lento en este caso
	let l3 = List.init 25_000 (fun _ -> Random.int 10000000);;
		# crono (qsort1 (<)) l3;;                                   
		- : float = 0.0497499999999995168
		# crono (qsort2 (<)) l3;;                                   
		- : float = 0.0506530000000005
	en listas más pequeñas la diferencia es muy poco apreciable
*)
