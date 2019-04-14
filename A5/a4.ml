open A1
exception Var_Not_found of string
exception Type_error
exception Definition_Error
exception Mult_def_in_Parallel


let rec findType l s = 
	match l with 
	[] -> raise (Var_Not_found(s))
	| (a,b)::xs -> ( if(a=s) then b else (findType xs s))
;;


let rec process l f =
	match l with 
	[] -> []
	| x::xs -> (f x) :: (process xs f)
;;


let rec intersection_val v l =
	match l with
	[] -> false
	| (a,b)::xs -> if (a=v) then true else (intersection_val v xs)
;;


let rec intersection_list l1 l2 = 
	match l1 with 
	[] -> false
	| (a,b)::xs -> if (intersection_val a l2) then true else (intersection_list xs l2)
;;

let rec defBinding g d = 
	match d with
	Simple(s,t,ex) -> if ((getType g ex)=t) then [(s,t)] else (raise Type_error)
	| Sequence(exlist)  -> (match exlist with
							[] -> []
							| x::xs -> (let g1 = (defBinding g x) in 
												( (defBinding (g1@g) (Sequence(xs))) @ g1)  )   )
	| Parallel(exlist)  -> (match exlist with
							[] -> []
							| x::xs -> (let (g1,gparallel) = (defBinding g x,defBinding g (Parallel(xs))) in
											if (intersection_list g1 gparallel) then (raise Mult_def_in_Parallel) else (g1@gparallel) ) )
												 
	| Local(d1,d2) -> ( defBinding ((defBinding g d1)@g) d2  )

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
and getType g e = 
	match e with 
	Var(s) -> (findType g s)
| 	N(n) -> Tint
|   B(b) -> Tbool
|   Abs(ex) | Negative(ex) -> if ((getType g ex) = Tint) then Tint else (raise Type_error)
|   Add(ex1,ex2) | Sub(ex1,ex2) | Mult(ex1,ex2) | Div(ex1,ex2) | Rem(ex1,ex2) -> if (((getType g ex1) = Tint) && ((getType g ex2) = Tint) ) then Tint else (raise Type_error)
|   Conjunction(ex1,ex2) | Disjunction(ex1,ex2) -> if (((getType g ex1) = Tbool) && ((getType g ex2) = Tbool) ) then Tbool else (raise Type_error)
|   Not(ex) -> if ((getType g ex) = Tbool) then Tbool else (raise Type_error)
|   Equals(ex1,ex2) | GreaterTE(ex1,ex2) | GreaterT(ex1,ex2) | LessT(ex1,ex2) | LessTE(ex1,ex2) -> if (((getType g ex1) = Tint) && ((getType g ex2) = Tint) ) then Tbool else (raise Type_error)
| 	InParen(ex1) -> (getType g ex1)
|   IfThenElse(ex1,ex2,ex3) -> if ( ((getType g ex1) = Tbool) && ((getType g ex2) = (getType g ex3))) then (getType g ex3) else (raise Type_error)
| 	Tuple(n,exlist) -> if ( n = 0 )  then Tunit else Ttuple(process exlist (getType g))
|   Project((i,n),ex) -> (	match (getType g ex) with 
								Ttuple(typelist) -> (List.nth typelist (i-1))
								| _ -> raise Type_error						)
| 	Let(d,ex) -> (getType ((defBinding g d)@g) ex)
|   FunctionAbstraction(s,t,ex) ->  Tfunc(t, getType ((s,t)::g) ex)
|   FunctionCall(ex1,ex2) ->( match (getType g ex1) with 
									Tfunc(t1,t2) -> (if (hastype g ex2 t1) then t2 else (raise Type_error))
									| _ -> (raise Type_error))

and hastype g e t = try ( t = (getType g e)   ) with 
	(Type_error) -> false
	| Var_Not_found(a) -> false
;;



let rec remove_string l s_name = 														(* returns (element_list, modified_list)*)
	match l with 
	[] -> []
	| x::xs -> (   match x with (s,_) -> 
 					   (if(s=s_name) then [] else [x])@(remove_string xs s_name)    )
;;

let rec modified_list l = 
	match l with 
	[] -> []
	| x::xs -> (match x with (s,_) ->
				x::(modified_list (remove_string xs s)))
;;


let compare_function a b = if (a>b) then 1 else if (a<b) then -1 else 0;; 

	

(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = ( (List.sort compare_function (modified_list (defBinding g d))) = (List.sort compare_function (modified_list g_dash)) );;





