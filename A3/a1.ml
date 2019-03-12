(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Type_error
exception Invalid_index_requested
exception Invalid_breaking
(* abstract syntax *)
type  exptree =  
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)


let rec evalList2 f1 f2 l =                            (*takes a list, applies the function f to all elements and returns the list*)
  match l with 
  [] -> []
| (x::xs) -> (f1 x f2)::(evalList2 f1 f2 xs)
;;

let rec evalList f l =                            (*takes a list, applies the function f to all elements and returns the list*)
  match l with 
  [] -> []
| (x::xs) -> (f x)::(evalList f xs)
;;


let rec getInd i l =                               (*returns ith element of the list, where list is 1 indexed*)
  match l with x::xs -> (if (i==1) then x else (getInd (i-1) xs)) | [] -> raise Invalid_index_requested
;;

let rec breakList l len =                             (*breaks list into 2 parts such that the first part has length len*)
    match l with x::xs ->
    (
      if (len==0) then 
            (([],l))
      else
          let temp=(breakList xs (len-1)) in
              match temp with (l1,l2) ->
                  (x::l1, l2)
    )
    | [] -> (if (len==0) then ([],[]) else raise Invalid_breaking)
;;

let rec compileList f l = 
  match l with
  [] -> [] 
| (x::xs) -> (f x)@(compileList f xs)
;;

let getAbs n = if(n>0) then n else -n;;

let rec eval (ex:exptree) (rho:(string->value)):value = 
  match ex with
  Var(s) -> (rho s)
| N(n) -> NumVal(n)
| B(b) -> BoolVal(b)
| Abs(ex1) -> 
          (let a1 = (eval ex1 rho) in
          match a1 with
          NumVal(b1) -> NumVal(getAbs b1) | _ -> raise Type_error)
| Negative(ex1) -> 
          (let a1 = (eval ex1 rho) in
          match a1 with
          NumVal(b1) -> NumVal(-b1) | _ -> raise Type_error)
| Not(ex1)  ->
          (let a1 = (eval ex1 rho) in
          match a1 with
          BoolVal(b1) -> BoolVal(not b1) | _ -> raise Type_error)
| Add(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> NumVal(b1 + b2)| _ -> raise Type_error)
| Sub(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> NumVal(b1 - b2)| _ -> raise Type_error)
| Mult(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> NumVal(b1 * b2)| _ -> raise Type_error)
| Div(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> NumVal(b1 / b2)| _ -> raise Type_error)
| Rem(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> NumVal(b1 mod b2)| _ -> raise Type_error)
| Conjunction(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1 && b2)| _ -> raise Type_error)
| Disjunction(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1 || b2)| _ -> raise Type_error)
| Equals(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> BoolVal(b1 == b2)| _ -> raise Type_error)
| GreaterTE(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> BoolVal(b1 >= b2)| _ -> raise Type_error)
| LessTE(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> BoolVal(b1 <= b2)| _ -> raise Type_error)
| GreaterT(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> BoolVal(b1 > b2)| _ -> raise Type_error)
| LessT(ex1,ex2) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          match (a1,a2) with
          (NumVal(b1),NumVal(b2)) -> BoolVal(b1 < b2)| _ -> raise Type_error)
| InParen(ex1) -> (eval ex1 rho)
| IfThenElse(ex1,ex2,ex3) ->
          (let a1 = (eval ex1 rho) in
          let a2 = (eval ex2 rho) in
          let a3 = (eval ex3 rho) in
          match a1 with
          BoolVal(b1) -> (if b1 then a2 else a3)| _ -> raise Type_error)
| Tuple(n, ex_list) -> TupVal((n,(evalList2 eval rho ex_list)))
| Project((i, n), ex) -> 
          (match ex with Tuple(n,ex_list) ->
              eval (getInd i ex_list) rho | _ -> raise Type_error)
;;


let rec compile (ex:exptree):opcode list =
match ex with
  Var(s) -> [VAR(s)]
| N(n) -> [NCONST(mk_big(n))]
| B(b) -> [BCONST(b)]
| Abs(ex1) -> (compile ex1)@[ABS]
| Negative(ex1) -> (compile ex1)@[UNARYMINUS]
| Not(ex1)  -> (compile ex1)@[NOT]
| Add(ex1,ex2) -> (compile ex1)@(compile ex2)@[PLUS]
| Sub(ex1,ex2) -> (compile ex1)@(compile ex2)@[MINUS]
| Mult(ex1,ex2) -> (compile ex1)@(compile ex2)@[MULT]
| Div(ex1,ex2) -> (compile ex1)@(compile ex2)@[DIV]
| Rem(ex1,ex2) -> (compile ex1)@(compile ex2)@[REM]
| Conjunction(ex1,ex2) -> (compile ex1)@(compile ex2)@[CONJ]
| Disjunction(ex1,ex2) -> (compile ex1)@(compile ex2)@[DISJ]
| Equals(ex1,ex2) -> (compile ex1)@(compile ex2)@[EQS]
| GreaterTE(ex1,ex2) -> (compile ex1)@(compile ex2)@[GTE]
| LessTE(ex1,ex2) -> (compile ex1)@(compile ex2)@[LTE]
| GreaterT(ex1,ex2) -> (compile ex1)@(compile ex2)@[GT]
| LessT(ex1,ex2) -> (compile ex1)@(compile ex2)@[LT]
| InParen(ex1)  -> (compile ex1)@[PAREN]
| IfThenElse(ex1,ex2,ex3) -> (compile ex3)@(compile ex2)@(compile ex1)@[IFTE]
| Tuple(n,ex_list) -> (List.rev (compileList compile ex_list))@[TUPLE(n)]
| Project((i,n),ex1) -> (compile ex1)@[PROJ(i,n)]
;;


let rec stackmc (stk:answer list) (rho: string->answer) (pgm:opcode list) : answer=
  match pgm with      (*case when pgm is empty*)
  [] -> (List.hd stk) |                                     
  x::xs -> 
    match x with 
   NCONST(a)->(stackmc ((Num(a))::stk) rho xs)                       (*element is appended to stack*)  
|  BCONST(a)->(stackmc ((Bool(a))::stk) rho xs)                       (*element is appended to stack*) 
|  VAR(a)   ->(stackmc ((rho a)::stk) rho xs)
|  ABS -> 
            (match stk with (op1::xss) -> 
                  match op1 with Num(a1) ->
                      stackmc ((Num(abs a1))::xss) rho xs| _ -> raise Type_error)
|  UNARYMINUS -> 
            (match stk with (op1::xss) -> 
                  match op1 with Num(a1) ->
                      stackmc ((Num(minus a1))::xss) rho xs| _ -> raise Type_error)
|  NOT -> 
            (match stk with (op1::xss) -> 
                  match op1 with Bool(a1) ->
                      stackmc ((Bool(not a1))::xss) rho xs| _ -> raise Type_error)
|  PLUS -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                (*binary operations are done in the order op2 <operation> op1*)
                      stackmc ((Num(add a2 a1))::xss) rho xs| _ -> raise Type_error)                           (*op2 is the 2nd highest element in stack*)
|  MINUS -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Num(sub a2 a1))::xss) rho xs| _ -> raise Type_error)                           
|  MULT -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Num(mult a2 a1))::xss) rho xs| _ -> raise Type_error) 
|  DIV -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Num(div a2 a1))::xss) rho xs| _ -> raise Type_error)                          
|  REM -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Num(rem a2 a1))::xss) rho xs| _ -> raise Type_error)             
|  CONJ -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Bool(a1),Bool(a2)) ->                                
                      stackmc ((Bool(a2 && a1))::xss) rho xs| _ -> raise Type_error)       
|  DISJ -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Bool(a1),Bool(a2)) ->                                
                      stackmc ((Bool(a2 || a1))::xss) rho xs| _ -> raise Type_error)    
|  EQS -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Bool(eq a2 a1))::xss) rho xs| _ -> raise Type_error) 
|  GTE -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Bool(geq a2 a1))::xss) rho xs| _ -> raise Type_error)                                      
|  LTE -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Bool(leq a2 a1))::xss) rho xs| _ -> raise Type_error) 
|  GT  -> 
            (match stk with op1::(op2::xss) ->
              match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Bool(gt a2 a1))::xss) rho xs| _ -> raise Type_error) 
|  LT  -> 
            (match stk with op1::(op2::xss) ->
               match (op1,op2) with (Num(a1),Num(a2)) ->                                
                      stackmc ((Bool(lt a2 a1))::xss) rho xs| _ -> raise Type_error) 
|  PAREN -> stackmc stk rho xs
|  IFTE ->  (match stk with op1::(op2::(op3::xss)) ->
               match op1 with Bool(a1) ->                                
                    stackmc  ((if a1 then op2 else op3)::xss) rho xs| _ -> raise Type_error)
|  TUPLE(n) -> 
            ( let () = Printf.printf "%d HERE %d. " n (List.length stk)  in 
              let a1 = (breakList stk n) in
                match a1 with (l1,l2) ->
                    stackmc ((Tup(n,l1))::l2) rho xs| _ -> raise Type_error)
|  PROJ(i,n) ->  
            (match stk with op1::xss ->
                match op1 with Tup(n,l1) ->
                    stackmc ((getInd i l1)::xss) rho xs| _ -> raise Type_error)
;;

