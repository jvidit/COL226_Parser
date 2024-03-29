(* Type definition for the expression parse tree generated by the parser *)
type expr_tree = NULL 
| VAR of string 
| NUM of int 
| PLUS of expr_tree * expr_tree 
| SUB of expr_tree * expr_tree 
| MOD of expr_tree * expr_tree 
| DIV of expr_tree * expr_tree 
| INTO of expr_tree * expr_tree 
| ARITH_PARENTHESIS of expr_tree 
| NEG of expr_tree 
| ABS of expr_tree
| BOOL_PARENTHESIS of expr_tree
| TRUE
| FALSE
| AND of expr_tree * expr_tree 
| OR of expr_tree * expr_tree 
| NOT of expr_tree

;;

exception Empty 
(* Map from string variable names to their integer values. New values can be added *)
module VarTable = Map.Make(String)
let variable_set = VarTable.(empty |> add "ten" 10 |> add "hundred" 100);;

(* Function to evaluate value given the parse tree *)
let rec eval_tree t = match t with
    NULL           -> raise Empty
    | NUM(x)       -> x
    | INTO(t1,t2)  -> (eval_tree t1) * (eval_tree t2)
    | PLUS(t1,t2)  -> (eval_tree t1) + (eval_tree t2)
    | DIV(t1,t2)  -> (eval_tree t1) / (eval_tree t2)
    | MOD(t1,t2)  -> (eval_tree t1) mod (eval_tree t2)
    | SUB(t1,t2)  -> (eval_tree t1) - (eval_tree t2)
    | ABS(x)      -> let temp = (eval_tree x) in if (temp>=0) then temp else -temp
    | NEG(x)      -> -1*(eval_tree x)
    | VAR(x)      -> VarTable.find x variable_set
    | ARITH_PARENTHESIS(x) -> (eval_tree x)

    | BOOL_PARENTHESIS(x) -> (eval_tree x)
    | TRUE ->   true
    | FALSE -> false
    | AND(t1,t2)  -> (eval_tree t1) && (eval_tree t2) 
    |  OR(t1,t2)  -> (eval_tree t1) || (eval_tree t2)
    | NOT(x)      -> not(eval_tree x)
;;

(* Function to print the expression tree, each node labelled with its level/depth *)
let rec print_tree t level = match t with
    NULL           -> Printf.printf "Empty Tree\n";
    | NUM(x)       -> Printf.printf "Level %d INT %d " level x;
    | INTO(t1,t2)  -> Printf.printf "Level %d *\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | PLUS(t1,t2)  -> Printf.printf "Level %d +\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | SUB(t1,t2)  -> Printf.printf "Level %d -\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | DIV(t1,t2)  -> Printf.printf "Level %d div\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | MOD(t1,t2)  -> Printf.printf "Level %d mod\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | ABS(t1)  -> Printf.printf "Level %d abs\n" level; print_tree t1 (level+1); print_newline();
    | NEG(t1)  -> Printf.printf "Level %d ~\n" level; print_tree t1 (level+1); print_newline();
    | VAR(x)       -> Printf.printf "Level %d INT %d " level (VarTable.find x variable_set);
    | ARITH_PARENTHESIS(x) -> (print_tree x level); 
;;

(* TODO
 * - Try writing a compile function that converts given parse tree into a postfix code.
 *    You might have to define a new type
 * - Try evaluating compiled expression tree using stack machine
 * *)
