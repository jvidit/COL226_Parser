
exception Var_not_found of char;;
exception Var_not_assigned of char;;
exception Not_implemented;;


type cmd = Var of char
|  N of int
|  Let of (char * cmd)
|  Call of (char * cmd * cmd)
|  Ret
|  Showpr
|  Showvr
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


let proc_list = [proc_V;proc_W;proc_R;proc_S;proc_T;proc_U;proc_P;proc_Q;proc_main];;




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


let rec lookup (var : char) (fr : frame) : var_val = 
    match fr with 
        MainFrame(Main(gamma, proc_list))  ->  (find_val var gamma)
        |  Frame(parent_frame,Proc(proc_name,gamma,p_list)) -> if (has_defined_var var gamma) then 
                                                                    (find_val var gamma)           (*retrieving variable value from this particular frame*)
                                                                                      else
                                                                    (lookup var parent_frame)   (*retrieving variable value from some ancestor frame*)
;;


(*returns value of variables : always returns a cmd of form Num(n)*)
let rec getVal (a : cmd) (fr : frame) : var_val = 
    match a with
      N(n) -> Num(n)
      | Var(v) -> (lookup v fr)
;;







(* SETTING VALUE OF A VARIABLE*)
let rec upd_val (var : char) (vall : var_val) (gamma : hashtable) : hashtable =                   (* takes a variable name, value, and a list. updates the value of the variable in the list*)
    match gamma with
      (c,x)::xs -> if (var = c) then ((c,vall)::xs) else ((c,x)::(upd_val var vall xs))       (*val is always of the form Num(a)*)
      | [] -> raise (Var_not_found(var))
;;



let rec set_var (var : char) (vall : var_val) (fr : frame) : frame = 
    match fr with 
        MainFrame(Main(gamma, proc_list))  ->  MainFrame(Main(upd_val var vall gamma,proc_list))
        |  Frame(parent_frame,Proc(proc_name,gamma,p_list)) -> if (has_var var gamma) then 
                                                                    (Frame(parent_frame,Proc(proc_name,upd_val var vall gamma,p_list)))           (*setting variable value in this particular frame*)
                                                                                      else
                                                                    (Frame(set_var var vall parent_frame,Proc(proc_name,gamma,p_list)))                (*setting variable value in some ancestor frame*)
;;






let rec eval (command:cmd) (stack:frame list) = match command with 
    Let(c,vall) -> (set_var c (getVal vall (List.hd stack)) (List.hd stack))::(List.tl stack) 
    | Showvr | Showpr -> stack 
    | _ -> raise Not_implemented
;;
  (* | Call(func,a,b) -> (match (getVal a,getVal b) with (Num(n1),Num(n2)) -> (stack, gamma)) *)














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




let rec show_values (command : cmd) (stack : frame list) : string = match command with
    Showvr ->  (get_all_vars (List.hd stack))
    | _ -> "\n"
;;  









