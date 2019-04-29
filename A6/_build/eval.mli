
exception Var_not_found of char;;
exception Var_not_assigned of char;;


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


type var_val = Num of int | ND ;; (* push a random variable called break onto stack: while dereferencing, delete till break*)

type hashtable = (char * var_val) list;;

type proc = Proc of (char * hashtable * proc list) | Main of (hashtable * proc list);; (*name,arg1,arg2,variable list,children procedure*)

type frame = Frame of (frame * proc) | MainFrame of proc;;                            (*  Frame ( parent * current procedure)  *)


val eval: cmd -> (frame list) -> (frame list);;

val show_values : cmd -> (frame list) -> string;;