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
| x::xs ->  (f x) ^ "," ^ (joinString f xs);;




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
  | TupVal(l, a) -> " TUP (" ^ (joinString print_value a) ^ ")"
;;

exception Not_implemented
(* Helper function to print *)
let rec foldp f l a rho=match l with
[]->Printf.printf "";
|x::xs->f x a rho;foldp f xs a rho;;
let rec print_tree t level rho= match t with
    N(x)       -> Printf.printf "Level %d INT %d " level x;print_newline();
    |B(x)->Printf.printf "Level %d %b " level x;print_newline();
    | Mult(t1,t2)  -> Printf.printf "Level %d *\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | Add(t1,t2)  -> Printf.printf "Level %d +\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | Sub(t1,t2)  -> Printf.printf "Level %d -\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | Rem(t1,t2)  -> Printf.printf "Level %d mod\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | Div(t1,t2)  -> Printf.printf "Level %d div\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | Abs(t1)  -> Printf.printf "Level %d absolute\n" level; print_tree t1 (level+1) rho; print_newline();
    | Negative(t1)  -> Printf.printf "Level %d negative\n" level; print_tree t1 (level+1) rho; print_newline();
    | Equals(t1,t2)  -> Printf.printf "Level %d eq\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | GreaterTE(t1,t2)  -> Printf.printf "Level %d geq\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | GreaterT(t1,t2)  -> Printf.printf "Level %d gt\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | LessTE(t1,t2)  -> Printf.printf "Level %d leq\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | LessT(t1,t2)  -> Printf.printf "Level %d lt\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | Not(t1)  -> Printf.printf "Level %d not\n" level; print_tree t1 (level+1) rho; print_newline();
    | Disjunction(t1,t2)  -> Printf.printf "Level %d or\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    | Conjunction(t1,t2)  -> Printf.printf "Level %d and\n" level; print_tree t1 (level+1) rho; print_tree t2 (level+1) rho; print_newline();
    |IfThenElse(a,b,c)-> Printf.printf "Level %d if_then_else_fi\n" level; print_tree a (level+1) rho;
    print_tree b (level+1) rho; print_tree c (level+1) rho; print_newline();
    |Tuple(a,xs)->Printf.printf "Level %d Tuple with %d elements\n" level a; foldp print_tree xs (level+1) rho; print_newline();
    |Project((a,b),t)->Printf.printf "Level %d Projection of %d element of tuple with %d elements\n" level a b; 
    print_tree t (level+1) rho; print_newline();
    |InParen(t)->Printf.printf "Level %d InParenthesis\n" level; print_tree t (level+1) rho; print_newline();
    |_->Printf.printf "??\n";
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
|  "Ttrue" -> NumVal 47
|  _ -> raise Not_implemented;;


(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser s rho = 
  let result = A3.main A2.read (Lexing.from_string s) in
  (print_tree result 0 rho);
  Printf.printf "%s\n" (print_value (A1.eval result rho));
  Printf.printf "%s\n" (print_answer ((A1.stackmc [] (binding rho) (A1.compile result))));;
    


let a = (parser "2-3+4" rho);;



