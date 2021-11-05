let f x = x
let h (x,y) = x
let i (x,y) = y
let j x = [x]
(*
No existe un número máximo de funciones de un tipo
que se pueden escribir, aunque es cierto que en los tres 
primeros casos las posibles funciones son formas diferentes 
de escribir la misma. Por ejemplo:
let f2 x = fst (x,x)
let h2 = fst
let i2 = snd 
let j2 x = [x;x;x]
*)
