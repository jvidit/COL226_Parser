(* Dummy implementation of A1 *)
open A0
exception Not_implemented




type  exptree =  N of int
  | Plus of exptree * exptree
  | Minus of exptree * exptree
  | Mult of exptree * exptree
  | Div of exptree * exptree
  | Rem of exptree * exptree
  | Nega of exptree
  | Abs of exptree
;;

type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS;;


(*
let rec eval (ex:exptree):bigint = 
	match ex with
	N(a) -> a|
	Plus(ex1,ex2)->(add (eval ex1) (eval ex2))|
	Minus(ex1,ex2)->(sub (eval ex1) (eval ex2))|
	Mult(ex1,ex2)->(mult (eval ex1) (eval ex2))|
	Div(ex1,ex2)->(div (eval ex1) (eval ex2))|
	Rem(ex1,ex2)->(rem (eval ex1) (eval ex2))|
	Nega(ex1)->(minus (eval ex1))|
	Abs(ex1)->(abs (eval ex1))
;;
*)


(*calculates child/children exptree(s) recursively and performs the operation depending what type the exptree is; base case being N of integer*)
let rec eval (ex:exptree):int = 
	match ex with
	N(a) -> a|
	Plus(ex1,ex2)->(eval ex1)+(eval ex2)|
	Minus(ex1,ex2)->(eval ex1)-(eval ex2)|
	Mult(ex1,ex2)->(eval ex1)*(eval ex2)|
	Div(ex1,ex2)->(eval ex1)/(eval ex2)|
	Rem(ex1,ex2)->(eval ex1) mod (eval ex2)|
	Nega(ex1)-> -(eval ex1)|
	Abs(ex1)->let temp=(eval ex1) in if(temp>0) then temp else -temp
;;



(*generates an opcode list of a given exptree by recursively traversing it in a post order fashion*)
let rec compile (ex:exptree):opcode list =
	match ex with
	N(a) -> [CONST( mk_big a)]|
	Plus(ex1,ex2)->(compile ex1)@(compile ex2)@[PLUS]|
	Minus(ex1,ex2)->(compile ex1)@(compile ex2)@[MINUS]|
	Mult(ex1,ex2)->(compile ex1)@(compile ex2)@[TIMES]|
	Div(ex1,ex2)->(compile ex1)@(compile ex2)@[DIV]|
	Rem(ex1,ex2)->(compile ex1)@(compile ex2)@[REM]|
	Nega(ex1)->(compile ex1)@[UNARYMINUS]|
	Abs(ex1)->(compile ex1)@[ABS]
;;


(*uses the opcode list generated from the compile function and performs evaluation in a tail recursive manner*)
let rec stackmc (stk:bigint list) (pgm:opcode list): bigint =							   (*head of list stk is treated as the top of the stack*)
	match pgm with			(*case when pgm is empty*)
	[] -> (List.hd stk) | 																    
	x::xs -> 
		match x with 
		CONST(a)->(stackmc (a::stk) xs)|     										(*element is appended to stack*)			
		PLUS -> (match stk with	op1::(op2::xss) ->stackmc ((add op2 op1)::xss) xs)|        (*op1 is the top element of the stack*)
		TIMES ->(match stk with	op1::(op2::xss) ->stackmc ((mult op2 op1)::xss) xs)|       (*op2 is the 2nd highest element in stack*)
		MINUS ->(match stk with	op1::(op2::xss) ->stackmc ((sub op2 op1)::xss) xs)|        (*binary operations are done in the order op2 <operation> op1*)
		DIV -> (match stk with	op1::(op2::xss) ->stackmc ((div op2 op1)::xss) xs)|        (*unary operations are done in the order <operation> op1*)
		REM -> (match stk with	op1::(op2::xss) ->stackmc ((rem op2 op1)::xss) xs)|
		ABS -> (match stk with	op1::xss ->stackmc ((abs op1)::xss) xs)|  				   
		UNARYMINUS -> (match stk with op1::xss ->stackmc ((minus op1)::xss) xs) 
;;