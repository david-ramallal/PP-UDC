let rec power x = function
  | 0 -> 1
  | y -> x * power x (y - 1) 
  
let rec power' x = function
  | 0 -> 1
  | y -> 
    if(y mod 2 = 0) then power' (x * x) (y / 2)
    else x * power' (x * x) (y / 2)
    
(*
La función power' debería ser mejor en términos de eficiencia
ya que la recursividad avanza de forma más rápida (y/2 en 
lugar de y-1)
Es probable que mereciera más la pena una versión que permitiera
el uso de floats, en vez de una que solo admita enteros, ya que
cuanta mayor variedad de valores disponibles, mejor
*)

let rec powerf x = function
  | 0 -> 1.
  | n -> 
    if(n mod 2 = 0) then powerf (x *. x) (n / 2)
    else x *. powerf (x *. x) (n / 2)
    
