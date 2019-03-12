(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}



(*regex used*)
let whitespace=[' ' '\t']
let digit = ['0'-'9']                   (*regex for any number with digits 0-9*)
let integer = ['-']? (['1'-'9']digit* | '0')   (*either integer is 0, or a integer with no leading zeros. Sign is optional*)
let lower_letter = ['a'-'z']                (*any string with lower case letters*)
let upper_letter = ['A'-'z']                (*any string with lower case letters*)
let letter = lower_letter | upper_letter               (*any string with letters of both cases*)                          

let identifier = upper_letter (letter | digit | '_' | '\'')*       (*any string beginning with lower case letter followed by alphanumeric*)




(* Creating a lexing rule for the type token defined in parser *)
rule token = parse
    [' ' '\t' '\n']  { token lexbuf }  (* skip whitespace *)
  | ';'            { EOL }    (* to demarcate end of each expression *)
  |'('             { LP }
  |')'             { RP }
  | '+'            { PLUS }
  | '-'            { SUB }
  | '*'            { MUL }
  | "div"          { DIV }
  | "mod"          { MOD }
  | '~'            { NEG }
  | "abs"          { ABS }


  | 'T'            { TRUE }
  | 'F'            { FALSE}
  | "not"          { NOT }
  | ('/' '\\' )    { AND }
  | ('\\' '/')     { OR }

  | integer as num_int  { INT (int_of_string num_int)    }  (* Token for integer type *)
  | identifier as id_str        { ID (id_str) }  (* Token for variable string *)
  | eof {EOF}
