type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp
;;  
let rec eval ctx = function
	Const b -> b
	| Var s -> List.assoc s ctx
	| Neg e -> not (eval ctx e)
	| Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
	| Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
	| Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
	| BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);;
;;

type oper = Not
;;
type biOper = Or | And | If | Iff
;;
type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop
;;

let rec prop_of_log_exp = function
	Const x -> C x
	| Var x -> V x
	| Neg x -> Op (Not, prop_of_log_exp x)
	| Disj (x,y) -> BiOp (Or, prop_of_log_exp x, prop_of_log_exp y)
	| Conj (x,y) -> BiOp (And, prop_of_log_exp x, prop_of_log_exp y)
	| Cond (x,y) -> BiOp (If, prop_of_log_exp x, prop_of_log_exp y)
	| BiCond (x,y) -> BiOp (Iff, prop_of_log_exp x, prop_of_log_exp y)
;;	
let rec log_exp_of_prop = function
	C x -> Const x
	| V x -> Var x
	| Op (Not, x) -> Neg (log_exp_of_prop x)
	| BiOp (Or, x, y) -> Disj (log_exp_of_prop x, log_exp_of_prop y)
	| BiOp (And, x, y) -> Conj (log_exp_of_prop x, log_exp_of_prop y)
	| BiOp (If, x, y) -> Cond (log_exp_of_prop x, log_exp_of_prop y)
	| BiOp (Iff, x, y) -> BiCond (log_exp_of_prop x, log_exp_of_prop y)
;;

let opval = function
	Not -> not;;
let biopval = function
	Or -> (||)
	| And -> (&&)
	| If -> ...
	| Iff -> ...;;
