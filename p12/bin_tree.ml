type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;
  
(*
let rec sum = function
	Empty -> 0
	| Node (x, l, r) -> x + (sum l) + (sum r);;

let rec prod = function
	Empty -> 1.0
	| Node (x, l, r) -> x *. (prod l) *. (prod r);;
*)

let sum t = fold_tree (fun a b c -> a + b + c) 0 t;;

let prod t =  fold_tree (fun a b c -> a *. b *. c) 1. t;;

let size t = fold_tree (fun a b c -> 1 + b + c) 0 t;;

let inorder t = fold_tree (fun a b c -> b @ a::c) [] t;;

let mirror t = fold_tree (fun a b c -> Node(a, c, b)) Empty t;;

