#directory "_build";; (* Consider this folder when looking for files *)

#load "lexer.cmo";;
#load "parser.cmo";;
#load "eval.cmo";;


open Lexer;;
open Parser;;
open Eval;;



let exp_parser s = Parser.exp Lexer.read (Lexing.from_string s) ;;


let rec loop stack =
    match read_line () with
    | "exit" -> ()
    | s -> let parsed_exp = (exp_parser s) in
    			print_endline(Eval.show_values parsed_exp stack)
    								; (loop (Eval.eval parsed_exp stack))
;;







let temp_main = MainFrame(Main([('a',Num(6));('b',Num(2));('c',ND)],[]));;
let temp_q = Frame(temp_main,Proc('P',[('z',ND);('w',ND);('x',ND);('b',ND)],[]));;


let test_stack = [temp_q;temp_main];;

let rec start () = let () = (loop []) in ()
;;









(* Instructions to use *)


(* 

1. rlwrap ocaml
   #use "main.ml"
   let () = start ();;
2. first command should be setup
3. Each command separated by space
4. type exit to quit the loop

*)







(*Test cases*)
(* 1. 


Add the following to main.ml first

let temp_main = MainFrame(Main([('a',Num(6));('b',Num(2));('c',ND)],[]));;
let temp_q = Frame(temp_main,Proc('P',[('z',ND);('w',ND);('x',ND);('b',ND)],[]));;


*)






