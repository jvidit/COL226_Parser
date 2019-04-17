(* Dummy implementation of A1 *)
exception Not_implemented
exception Var_Not_found of string


(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

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
  | Let of definition * exptree
  | FunctionAbstraction of string * exptype * exptree
  | FunctionCall of exptree * exptree
(* definition *)
and definition =
    Simple of string * exptype * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition



(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of int | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE of ((opcode list) * (opcode list)) | TUPLE of int | PROJ of int*int | LET | FCALL | CLS of string * (opcode list) | RET
  | SIMPLEDEF of string | SEQCOMPOSE | PARCOMPOSE | LOCALDEF

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of int | Bool of bool | Tup of int * (answer list)



type table = (string * closure) list and
  closure = CL of (exptree * table) | VCL of (answer * table) | SecdCL of string * (opcode list) * table


let rec lookup l s = 
  match l with 
  [] -> raise (Var_Not_found(s))
  | (a,b)::xs -> ( if(a=s) then b else (lookup xs s))
;;





let rec krivine (c: closure) (clist : closure list) : closure =
  match c with 
    CL(Var(s),g) -> (krivine (lookup g s) clist)
    | CL(N(n),g) -> (VCL(Num(n), g))
    | CL(B(n),g) -> (VCL(Bool(n),g))


    | CL(Abs(e),g) -> ( match (krivine (CL(e,g)) clist) with 
                        (VCL(Num(a),g1)) -> VCL(Num(abs a),g))
    | CL(Negative(e),g) -> ( match (krivine (CL(e,g)) clist) with 
                        (VCL(Num(a),g1)) -> VCL(Num(-a),g))


    | CL(Not(e),g) -> ( match (krivine (CL(e,g)) clist) with 
                        (VCL(Bool(a),g1)) -> VCL(Bool(not a),g))


    | CL(Add(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Num(a1+a2),g))
    | CL(Sub(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Num(a1-a2),g))
    | CL(Mult(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Num(a1*a2),g))
    | CL(Div(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Num(a1/a2),g))
    | CL(Rem(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Num(a1 mod a2),g))

    | CL(Conjunction(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Bool(a1),g1)),(VCL(Bool(a2),g2))) -> VCL(Bool(a1 && a2),g))
    | CL(Disjunction(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Bool(a1),g1)),(VCL(Bool(a2),g2))) -> VCL(Bool(a1 || a2),g))


    | CL(Equals(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Bool(a1=a2),g))
    | CL(GreaterTE(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Bool(a1>=a2),g))
    | CL(LessTE(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Bool(a1<=a2),g))
    | CL(GreaterT(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Bool(a1>a2),g))
    | CL(LessT(e1,e2),g) -> ( match (krivine (CL(e1,g)) clist, krivine (CL(e2,g)) clist) with 
                        ((VCL(Num(a1),g1)),(VCL(Num(a2),g2))) -> VCL(Bool(a1<a2),g))


    | CL(InParen(e),g) -> (krivine (CL(e,g)) clist) 
                        
    | CL(IfThenElse(e1,e2,e3),g) -> ( match (krivine (CL(e1,g)) clist) with
                                    (VCL(Bool(a1),g1))  -> ( if a1 then (krivine (CL(e2,g)) clist) else (krivine (CL(e3,g)) clist) ) )

    | CL(FunctionAbstraction(x,t,e),g) -> (match clist with 
                                            cl::clists ->    (krivine (CL(e, (x,cl)::g )) clists  )   )
    | CL(FunctionCall(e1,e2),g)        -> (krivine (CL(e1,g))  (CL(e2,g)::clist))
    | _ -> raise Not_implemented
;;


let rec compileList f l = 
  match l with
  [] -> [] 
| (x::xs) -> (f x)@(compileList f xs)
;;


let rec compile (ex:exptree): opcode list =
match ex with
  Var(s) -> [VAR(s)]
| N(n) -> [NCONST(n)]
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
| IfThenElse(ex1,ex2,ex3) -> (compile ex1)@[IFTE((compile ex2),(compile ex3))]
| FunctionCall(ex1,ex2) -> (compile ex1)@(compile ex2)@[FCALL]
| FunctionAbstraction(x,t,ex1) -> [CLS(x,(compile ex1)@[RET])]


(* | Tuple(n,ex_list) -> ((compileList compile ex_list))@[TUPLE(n)]
| Project((i,n),ex1) -> (compile ex1)@[PROJ(i,n)]
| Let(d1,ex1) -> (compile ex1)@(compile_def d1)@[LET]

and rec compile_def (def:definition):opcode list = 
  match def with 
    Simple(s,t,ex) -> (compile ex)@[SIMPLEDEF(s)]
    |  Sequence(deflist) -> ( match deflist with 
                                [] -> []
                                | x::xs -> (compile_Def x)@[SEQCOMPOSE]@(compile_def Sequence(xs))  )
    |  Parallel(deflist) -> ( match deflist with 
                                [] -> []
                                | x::xs -> (compile_Def x)@[PARCOMPOSE]@(compile_def Sequence(xs))  )
    |  Localdef(d1,d2)  -> (compile_def d1)@(compile_def d2)@[LOCALDEF] *)

| _ -> raise Not_implemented
;;





let rec secd (stack : closure list) (env : table) (control : opcode list) (dump) = 
    match (stack, env, control, dump) with 
       
       (s,e,(VAR(x)::xs),d) -> (secd ((lookup e x)::s) e xs d)
       |  (s,e,NCONST(n)::xs,d) -> (secd (VCL(Num(n),e)::s) e xs d)
       |  (s,e,BCONST(n)::xs,d) -> (secd (VCL(Bool(n),e)::s) e xs d)
       |  (VCL(Num(a),_)::s,e,ABS::xs,d) -> (secd (VCL(Num(abs a),e)::s) e xs d)
       |  (VCL(Num(a),_)::s,e,UNARYMINUS::xs,d) -> (secd (VCL(Num(-a),e)::s) e xs d)
       |  (VCL(Bool(a),_)::s,e,NOT::xs,d) -> (secd (VCL(Bool(not a),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,PLUS::xs,d) -> (secd (VCL(Num(a1 + a2),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,MINUS::xs,d) -> (secd (VCL(Num(a2 - a1),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,MULT::xs,d) -> (secd (VCL(Num(a1 * a2),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,DIV::xs,d) -> (secd (VCL(Num(a2 / a1),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,REM::xs,d) -> (secd (VCL(Num(a2 mod a1),e)::s) e xs d)
       |  (VCL(Bool(a1),_)::(VCL(Bool(a2),_)::s),e,CONJ::xs,d) -> (secd (VCL(Bool(a1 && a2),e)::s) e xs d)
       |  (VCL(Bool(a1),_)::(VCL(Bool(a2),_)::s),e,DISJ::xs,d) -> (secd (VCL(Bool(a1 || a2),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,EQS::xs,d) -> (secd (VCL(Bool(a2 = a1),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,GT::xs,d) -> (secd (VCL(Bool(a2 > a1),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,GTE::xs,d) -> (secd (VCL(Bool(a2 >= a1),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,LT::xs,d) -> (secd (VCL(Bool(a2 < a1),e)::s) e xs d)
       |  (VCL(Num(a1),_)::(VCL(Num(a2),_)::s),e,LTE::xs,d) -> (secd (VCL(Bool(a2 <= a1),e)::s) e xs d)
       |  (s,e,PAREN::xs,d)   -> (secd s e xs d)
       |  (VCL(Bool(true),_)::s,e,(IFTE(a1,a2))::xs,d) -> (secd s e (a1@xs) d)
       |  (VCL(Bool(false),_)::s,e,(IFTE(a1,a2))::xs,d) -> (secd s e (a2@xs) d)
       |  (s,e,CLS(x,opl)::xs,d) ->  let () = Printf.printf "fabs\n" in (secd (SecdCL(x,opl,e)::s) e xs d)
       |  (a::((SecdCL(x,opl,e))::s),e1,FCALL::c,d) ->  let () = Printf.printf "fcall\n" in (secd [] ((x,a)::e) opl ((s,e1,c)::d))
       |  (a::s,e,RET::c,(s1,e1,c1)::d) -> (secd (a::s1) e1 c1 d)
       |  (a::s,_,[],_) -> (List.hd stack)
       |   _ -> raise Not_implemented
;;





    


