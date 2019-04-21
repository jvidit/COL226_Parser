#directory "_build";; (* Consider this folder when looking for files *)

#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;

open A1;;
open A2;;
open A3;;
open A4;;




exception Not_implemented
exception Definition_Error
(* Helper function to print *)
let rec print_tree tr = match tr with
  Var(s) -> s
  |N(i) -> "N " ^ (string_of_int i)
  |B(b) -> "B " ^ if b then "T" else "F"
  |Abs(e) -> "Abs( " ^ (print_tree e) ^ " )"
  |Negative(e) -> "Negative( " ^ (print_tree e) ^ " )"
  |Not(e) -> "Not( " ^ (print_tree e) ^ " )"
  |Add(e1, e2) -> "Add( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |Sub(e1, e2) -> "Sub( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |Mult(e1, e2) -> "Mult( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |Div(e1, e2) -> "Div( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |Rem(e1, e2) -> "Rem( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |Conjunction(e1, e2) -> "Conj( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |Disjunction(e1, e2) -> "Disj( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |Equals(e1, e2) -> "Eq( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
                      
  |GreaterTE(e1, e2) -> "Gte( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |LessTE(e1, e2) -> "Lte( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |GreaterT(e1, e2) -> "Gt( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |LessT(e1, e2) -> "Lt( " ^ (print_tree e1) ^ "," ^ (print_tree e2) ^ " )"
  |InParen(e) -> "( " ^ (print_tree e) ^ " )"
  |IfThenElse(e0, e1, e2) -> "if (" ^ (print_tree e0) ^ ") then (" ^ (print_tree e1) ^") else (" ^ (print_tree e2) ^ ") fi"
  |Tuple(n, l) -> "Tuple "
  |Project((a,b), e) -> "Proj( (" ^(string_of_int a) ^","^(string_of_int b) ^") "^(print_tree e) ^" )"
  |Let(d,e) -> "Let( " ^ (print_def d) ^ "," ^ (print_tree e) ^ " )"
  |FunctionAbstraction (s,t,e) -> "\\" ^ s ^ "." ^ (print_tree e)
  |FunctionCall(e1, e2) -> "Function Call " ^ (print_tree e1) ^" Calling parameter " ^ (print_tree e2)

and  print_def def = match def with
  Simple(s,t,e1) -> "Simple( " ^ s ^ "," ^ (print_tree e1) ^ " )"
  |Sequence(d::ds) -> "Sequence( " ^ (print_def d) ^ "," ^ (print_def_list ds) ^ " )"
  |Parallel(d::ds) -> "Parallel( " ^ (print_def d) ^ "," ^ (print_def_list ds) ^ " )"
  |Local(d1,d2) -> "Local( " ^ (print_def d1) ^ "," ^ (print_def d2) ^ " )"
  | _ -> raise Definition_Error

  and  print_def_list d = match d with 
  [] -> ""
  |[dd] -> (print_def dd)
  |dd::dds -> (print_def dd) ^ "," ^ (print_def_list dds)


let rec joinString f l = match l with
  [] -> ""
| x::xs ->  (f x) ^ "," ^ (joinString f xs);;

let rec print_answer tr = match tr with
    Num(a) -> (string_of_int a)
  | Bool(a) -> (string_of_bool a)
  | Tup(l, a) -> (joinString print_answer a)
;;


let rec print_value tr = match tr with
    NumVal(a) -> (string_of_int a)
  | BoolVal(a) -> (string_of_bool a)
  | TupVal(l, a) -> " TUP (" ^ (joinString print_value a) ^ ")"
;;




(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;

(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;;

(* Input is given as string and output is a value *)
let rho s = match s with
  "X" -> NumVal 5
  |  "Y" -> BoolVal true
  |  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
  | _ -> raise Not_implemented
;;


(* Sample test case *)





(* Type assumptions as a list of tuples of the form (variable name, type) *)
let g = [("Y", Tbool)];;
let e = exp_parser "let def F1:(Tint -> Tint) = \\X:Tint.(if (X=1) then 1 else (if (X=2) then 1 else ( F1((X-1)) + F1((X-2)) ) fi) fi) in  F1(10) end  " rho;;
(* let e1 = exp_parser "let def func = \\X:Tint.(X*X)(4)" rho;;   *)


(* assert(hasCompatibleType g e);; (* should return true *) *)


let makeClosure (c : closure) =
    match c with 
    VCL(Num(a),g) -> string_of_int a
    | VCL(Bool(a),g) -> string_of_bool a
    | _ -> "Not_implemented"
;;


let eval_krivine typeTable valTable e = 
    (* let _ = assert(hasCompatibleType typeTable e) in *)
    print_endline (makeClosure (krivine (CL(e,valTable)) [] ))
;;


let eval_secd typeTable valTable e = 
   (* let _ = assert(hasCompatibleType typeTable e) in *)
   print_endline (makeClosure (secd [] valTable (compile e) [] ))
;;


eval_krivine g [] e;; 





(* Sample parsing *)
(* print_endline (print_tree e2);;
print_endline (print_def d1);; *)
(* print_endline (print_tree (exp_parser "2>=4+5" rho));;  *)
(* print_endline (print_def (def_parser "def D1=3;def D2=6||def D3=(9*8)" rho));; *)


(* 
assert(hastype g e2 (Tfunc(Tint, Tint)));;
assert(yields g d1 g_dash);;

 *)



(*Test cases*)

(*(if F then (\\X:Tint.(3+X)) else (\\Y:Tint.(40+Y*Y)) fi)((31 div 3))    *)

(*let def X:Tint=2 in X+5 end*)

(*let def Func:(Tint -> Tint)  = \\X:Tint.(X*X) in Func(4) end*)

(*let def X:Tint=(3*4) in (X+5) end*)


(*let def F1:(Tint -> Tint) = \\X:Tint.(3*X) in   
                       let def F2:(Tint -> Tint) = \\X:Tint.( F1(F1(X)) ) in 
                                                 F2(5) end end               *)


(* Factorial let def F1:(Tint -> Tint) = \\X:Tint.(if (X=1) then 1 else (X*F1((X-1))) fi) in  F1(10) end *)

(* Fibonacci let def F1:(Tint -> Tint) = \\X:Tint.(if (X=1) then 1 else (if (X=2) then 1 else ( F1((X-1)) + F1((X-2)) ) fi) fi) in  F1(10) end *)




