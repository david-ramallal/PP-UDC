type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t))
;;
let rec height = function
	Gt (_,[]) -> 1
	| Gt (r,h::t) -> max (1 + height h) (height (Gt (r,t)))
;;
let rec leaves = function
	Gt (r,[]) -> [r]
	| Gt (r,h::[]) -> leaves h
	| Gt (r,h::t) -> leaves h @ leaves (Gt (r,t))
;;
(*Revisar mirror, no va bien del todo*)
let rec mirror = function
	Gt (r,[]) -> Gt (r, [])
	| Gt (r,h::[]) -> Gt (r, mirror h::[])
	| Gt (r,h::t) -> Gt (r,List.rev (mirror h::t))
;;
(*Revisar preorder, no va bien del todo*)
let rec preorder = function
	Gt (r,[]) -> [r]
	| Gt (r,h::[]) -> r :: preorder h
	| Gt (r,h::t) -> (r :: (preorder h)) @ (preorder (Gt (r,t)))
;;
let rec postorder = function
	Gt (r,[]) -> [r]
	| Gt (r,h::t) -> (postorder h) @ (postorder (Gt (r,t)))
;;



