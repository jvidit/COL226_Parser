
exception Var_not_found of char;;
exception Var_not_assigned of char;;
exception Not_implemented;;
exception Invalid_scoping;;
exception Function_not_found of char;;
exception Implemented_error;;

type cmd = Var of (int * char)
|  N of int
|  Let of (cmd * cmd)
|  Call of (char * cmd * cmd)
|  Ret
|  Showpr
|  Showvr
|  Showstk
|  Setup
;;





type var_val = Num of int | ND ;; 

type hashtable = (char * var_val) list;;

type proc = Proc of (char * hashtable * proc list) | Main of (hashtable * proc list);; (*name,arg1,arg2,variable list,children procedure*)

type frame = Frame of (frame * proc) | MainFrame of proc;;                            (*  Frame ( parent * current procedure)  *)


let proc_V = Proc('V',[('m',ND);('n',ND);('c',ND)],[]);;
let proc_W = Proc('W',[('m',ND);('p',ND);('j',ND);('h',ND)],[]);;
let proc_R = Proc('R',[('w',ND);('i',ND);('j',ND);('b',ND)],[proc_V]);;
let proc_S = Proc('S',[('c',ND);('k',ND);('m',ND);('n',ND)],[]);;
let proc_T = Proc('T',[('a',ND);('b',ND);('i',ND);('f',ND)],[proc_W]);;
let proc_U = Proc('U',[('c',ND);('z',ND);('p',ND);('g',ND)],[]);;
let proc_P = Proc('P',[('x',ND);('y',ND);('z',ND);('a',ND)],[proc_R;proc_S]);;
let proc_Q = Proc('Q',[('z',ND);('w',ND);('x',ND);('b',ND)],[proc_T;proc_U]);;
let proc_main = Main([('a',ND);('b',ND);('c',ND)],[proc_P;proc_Q]);;


let proc_list = [proc_V;proc_W;proc_R;proc_S;proc_T;proc_U;proc_P;proc_Q];;




let rec has_var (var : char) (gamma : hashtable) : bool =                 (* checks presence of variable in a list*)
    match gamma with
      (c,_)::xs -> if (var = c) then true else (has_var var xs)
      | [] -> false
;;

let rec has_defined_var (var : char) (gamma : hashtable) : bool =         (*checks presence of variable and whether it is defined in a list*)
    match gamma with
      (c,x)::xs -> if (var = c) then (match x with ND -> false | _ -> true) else (has_defined_var var xs)
      | [] -> false
;;

(*FINDING VALUE OF A VARIABLE*)

let rec find_val (var : char) (gamma : hashtable) : var_val =                                 (* takes a variable name, and a list. returns the value of the variable in the list*)
    match gamma with
      (c,x)::xs -> if (var = c) then x else (find_val var xs)
      | [] -> raise (Var_not_found(var))
;;


let rec lookup (var : cmd) (fr : frame) : var_val = 
    match (var,fr) with 
        (Var(n,v),MainFrame(Main(gamma, proc_list)))  -> if (n=0) then (find_val v gamma) else (raise Invalid_scoping)
        |  (Var(n,v),Frame(parent_frame,Proc(proc_name,gamma,p_list))) -> if (has_defined_var v gamma) then 
                                                                            ( if (n=0) then (find_val v gamma) else (lookup (Var(0,v)) parent_frame)) (*retrieving variable value from this particular frame*)
                                                                                      else
                                                                            (lookup var parent_frame)   (*retrieving variable value from some ancestor frame*)
;;


(*returns value of variables : always returns a cmd of form Num(n)*)
let rec getVal (a : cmd) (fr : frame) : var_val = 
    match a with
      N(n) -> Num(n)
      | Var(n,v) -> (lookup (Var(n,v)) fr)
;;







(* SETTING VALUE OF A VARIABLE*)
let rec upd_val (var : char) (vall : var_val) (gamma : hashtable) : hashtable =                   (* takes a variable name, value, and a list. updates the value of the variable in the list*)
    match gamma with
      (c,x)::xs -> if (var = c) then ((c,vall)::xs) else ((c,x)::(upd_val var vall xs))       (*val is always of the form Num(a)*)
      | [] -> raise (Var_not_found(var))
;;



let rec set_var (var : cmd) (vall : var_val) (fr : frame) : frame = 
    match (var,fr) with 
        (Var(n,v),MainFrame(Main(gamma, proc_list)))  ->  if (n=0) then (MainFrame(Main(upd_val v vall gamma,proc_list))) else (raise Invalid_scoping)
        |  (Var(n,v),Frame(parent_frame,Proc(proc_name,gamma,p_list))) -> if (has_var v gamma) then 
                                                                           ( if (n=0) then ((Frame(parent_frame,Proc(proc_name,upd_val v vall gamma,p_list))) ) else ((Frame(set_var (Var(0,v)) vall parent_frame,Proc(proc_name,gamma,p_list))) ) ) (*setting variable value in this particular frame*)
                                                                                      else
                                                                           (Frame(set_var var vall parent_frame,Proc(proc_name,gamma,p_list)))                (*setting variable value in some ancestor frame*)
;;




(*CALLING FUNCTION*)
let rec has_func (func_name : char) (plist : proc list) : bool =                            (* checks presence of function in a given function list*)
    match plist with
    Proc(pname,_,_)::xs -> if (pname = func_name) then true else (has_func func_name xs)
    | _ -> false
;;


let rec find_func_template (func_name : char) (plist : proc list) : proc =                  (*finds the function tempelate from proc_list(defined above) *)
    match plist with
    Proc(c,vlist,flist)::xs -> if ( c = func_name ) then (Proc(c,vlist,flist)) else (find_func_template func_name xs)
    | _ -> raise Implemented_error
;;

  
let bind_arg (f : proc) (arg1 : var_val) (arg2 : var_val) : proc =                                   (*takes proc tempelate and binds first 2 variables with argument values*) 
    match f with
    Proc(pname, (v1,n1)::((v2,n2)::xs) , proc_list) -> (Proc(pname, (v1,arg1)::((v2,arg2)::xs) , proc_list))
    | _ -> raise Implemented_error
;; 



let rec func_call (func_name : char) (arg1 : var_val) (arg2 : var_val) (fr : frame) : frame =                                    (*arg1 and arg2 are of the form Num(_)*)                                                   (*checks presence of a child function in a frame *)   
    match fr with 
    Frame(parent_frame,Proc(_,_,plist)) -> if (has_func func_name plist) then (Frame(fr,(bind_arg (find_func_template func_name proc_list) arg1 arg2))) else (func_call func_name arg1 arg2 parent_frame)
    | MainFrame(Main(_,plist)) -> if (has_func func_name plist) then (Frame(fr,(bind_arg (find_func_template func_name proc_list) arg1 arg2))) else (raise (Function_not_found func_name))
;;








let rec eval (command:cmd) (stack:frame list) : frame list = match command with 
    Let(c,vall) -> (set_var c (getVal vall (List.hd stack)) (List.hd stack))::(List.tl stack) 
    | Setup -> [MainFrame(proc_main)]
    | Call(c,e1,e2) ->  (match (getVal e1 (List.hd stack),getVal e2 (List.hd stack)) with (n1,n2) ->              (* n1 and n2 are function arguments*)
                                        (func_call c n1 n2 (List.hd stack))::stack)
    | Ret -> (List.tl stack)
    | Showvr | Showpr | Showstk -> stack 
    | _ -> raise Not_implemented
;;















(* Collect all variable values*)
let rec get_vars (gamma : hashtable) : string = 
    match gamma with
    [] -> ""
    | (c,Num(n))::xs -> ( " " ^ (String.make 1 c) ^ " = " ^ (string_of_int n) ^ "\n" ^ (get_vars xs))
    | (c,ND)::xs -> (get_vars xs)                   (* skipping undefined variables*)
;;



let rec get_all_vars (fr : frame) : string = 
    match fr with 
        MainFrame(Main(gamma, proc_list))  ->  ("From main \n" ^ (get_vars gamma))
        |  Frame(parent_frame,Proc(proc_name,gamma,p_list)) -> ((get_all_vars parent_frame) ^ "From " ^ (String.make 1 proc_name) ^ "\n" ^ (get_vars gamma))
;;



(* Collect all procedures which can be called *)
let rec get_procs (plist : proc list) : string = 
    match plist with
    [] -> ""
    | (Proc(c,_,_))::xs -> ((String.make 1 c) ^ " " ^ (get_procs xs))
;;



let rec get_all_procs (fr : frame) : string = 
    match fr with 
        MainFrame(Main(gamma, proc_list))  ->  (get_procs proc_list)
        |  Frame(parent_frame,Proc(proc_name,gamma,p_list)) -> ((get_all_procs parent_frame) ^ ((get_procs p_list)))
;;



(* Display call stack*)
let rec get_stack_call (stack : frame list) : string = 
    match stack with 
           (MainFrame(Main(gamma,_))::xs)  ->  "Main"
        |  (Frame(_,Proc(proc_name,_,_))::xs) -> (get_stack_call xs) ^ " -> " ^ (String.make 1 proc_name)
;;



let rec show_values (command : cmd) (stack : frame list) : string = match command with
    Showvr ->  (get_all_vars (List.hd stack))
    | Showpr -> (get_all_procs (List.hd stack)) ^ "\n"
    | Showstk -> (get_stack_call stack)
    | _ -> "Executed\n"
;;  











