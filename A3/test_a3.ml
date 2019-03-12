#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";; (* Load the a0 bytecode *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;
exception Not_implemented
exception Type_error
exception Invalid_index_requested

let rec getInd i l =                               (*returns ith element of the list, where list is 1 indexed*)
  match l with x::xs -> (if (i==1) then x else (getInd (i-1) xs)) | [] -> raise Invalid_index_requested
;;

let rec evalList f l =                            (*takes a list, applies the function f to all elements and returns the list*)
  match l with 
  [] -> []
| (x::xs) -> (f x)::(evalList f xs)
;;

let rec joinString f l = match l with
  [] -> ""
| x::xs ->  (f x) ^  (joinString f xs);;

exception Not_implemented
(* Helper function to print *)
let rec print_tree tr = match tr with
  N a -> " INT " ^ (string_of_int a)
| Var(s) -> "VAR " ^ s
| B(b) -> "bool " ^ (string_of_bool b)
| Abs(ex1) -> "abs " ^ (print_tree ex1)
| Negative(ex1) -> " " ^ (print_tree ex1)
| Not(ex1)  -> "~ " ^ (print_tree ex1)
| Add(ex1,ex2) -> "+ " ^ (print_tree ex1) ^ (print_tree ex2)
| Sub(ex1,ex2) -> "- " ^ (print_tree ex1) ^ (print_tree ex2)
| Mult(ex1,ex2) -> "* " ^ (print_tree ex1) ^ (print_tree ex2)
| Div(ex1,ex2) -> "div " ^ (print_tree ex1) ^ (print_tree ex2)
| Rem(ex1,ex2) -> "mod " ^ (print_tree ex1) ^ (print_tree ex2)
| Conjunction(ex1,ex2) -> "conj " ^ (print_tree ex1) ^ (print_tree ex2)
| Disjunction(ex1,ex2) -> "disj " ^ (print_tree ex1) ^ (print_tree ex2)
| Equals(ex1,ex2) -> "= " ^ (print_tree ex1) ^ (print_tree ex2)
| GreaterTE(ex1,ex2) -> ">= " ^ (print_tree ex1) ^ (print_tree ex2)
| LessTE(ex1,ex2) -> "<= " ^ (print_tree ex1) ^ (print_tree ex2)
| GreaterT(ex1,ex2) -> "> " ^ (print_tree ex1) ^ (print_tree ex2)
| LessT(ex1,ex2) -> "< " ^ (print_tree ex1) ^ (print_tree ex2)
| InParen(ex1) -> "() " ^ (print_tree ex1)
| IfThenElse(ex1,ex2,ex3) -> "if " ^ (print_tree ex1) ^ "then " ^ (print_tree ex2) ^ "else " ^ (print_tree ex3) ^ "fi "
| Tuple(n, ex_list) -> "tup " ^ (joinString print_tree ex_list)
| Project((i, n), ex) -> 
         ( match ex with Tuple(n,ex_list) ->
              ("Proj " ^ (print_tree (getInd i ex_list)) ^ (print_tree ex)) | _ -> raise Type_error)
;;




let rec print_answer tr = match tr with
    Num(a) -> (print_num a)
  | Bool(a) -> (string_of_bool a)
  | Tup(l, a) -> (joinString print_answer a)
;;

(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;




let rec print_value tr = match tr with
    NumVal(a) -> (string_of_int a)
  | BoolVal(a) -> (string_of_bool a)
  | TupVal(l, a) -> (joinString print_value a)
;;


(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;


(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;



(* Input is given as string and output is a value *)
let rho s = match s with 
   "X" -> NumVal 5
|  "Y" -> BoolVal true
|  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
|  _ -> raise Not_implemented;;


(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser s rho = 
  let result = A3.main A2.read (Lexing.from_string s) in
  Printf.printf "%s\n" (print_tree result);
  Printf.printf "%s\n" (print_value (A1.eval result rho));
  Printf.printf "%s\n" (print_answer ((A1.stackmc [] (binding rho) (A1.compile result))));;
    


let a = (parser "if (5>=3) then (  6+~(~(-7)))  ) else (T) fi "  rho);;



