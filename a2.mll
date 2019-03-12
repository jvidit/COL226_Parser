
(*definition of token*)
{
  exception Syntax_error of char   (*Error defined for unrecognised tokens*)
  type token =
   INT of int          (* integer constant, positive or negative w/o leading zeros *)
|  TRUE                (* boolean constant "T" *)
|  FALSE               (* boolean constant "F" *)
|  ABS                 (* unary operator, "abs" *)
|  PLUS                (* arithmetic plus, "+" *)
|  MINUS               (* arithmetic minus, "-" *)
|  MUL                 (* arithmetic multiply, "*" *)
|  DIV                 (* integer div, "div" *)
|  MOD                 (* remainder, "mod" *)
|  EXP                 (* exponentiation, "^" *)
|  LP                  (* left paren, "(" *)
|  RP                  (* right paren, ")" *)
|  NOT                 (* boolean NOT, "not" *)
|  AND                 (* boolean AND, "/\ " *)
|  OR                  (* boolean OR, "\/" *)
|  EQ                  (* equal to, "=" *)
|  GTA                 (* greater than, ">" *)
|  LTA                 (* less than, "<" *)
|  GEQ                 (* greater than/equal to, ">=" *)
|  LEQ                 (* less than/equal to, "<=" *)
|  IF                  (* keyword "if" *)
|  THEN                (* keyword "then" *)
|  ELSE                (* keyword "else" *)
|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
|  DEF                 (* definition construct, "def" *)
|  DELIMITER;;         (* delimiter, ";" *)
}


(*regex used*)
let whitespace=[' ' '\t']
let digit = ['0'-'9']										(*regex for any number with digits 0-9*)
let integer = ['-' '+']? (['1'-'9']digit* | '0')			(*either integer is 0, or a integer with no leading zeros. Sign is optional*)
let lower_letter = ['a'-'z']								(*any string with lower case letters*)
let letter = ['a'-'z' 'A'-'Z']								(*any string with letters of both cases*)														

let identifier = lower_letter (letter* digit*)*				(*any string beginning with lower case letter followed by alphanumeric*)



rule read = parse
  integer     as n   {INT (int_of_string n) :: (read lexbuf)}
| "abs"   	 	 	 {ABS  :: (read lexbuf)}
| "T"    	 		 {TRUE :: (read lexbuf)}
| "F"    	 		 {FALSE :: (read lexbuf)}
| "+"   	 		 {PLUS :: (read lexbuf)}
| "-"    	 		 {MINUS :: (read lexbuf)}
| "*"    	 		 {MUL :: (read lexbuf)}
| "div"    	 		 {DIV :: (read lexbuf)}
| "mod"    	 		 {MOD :: (read lexbuf)}
| "^"    	 		 {EXP :: (read lexbuf)}
| "("    	 		 {LP :: (read lexbuf)}
| ")"    	 		 {RP :: (read lexbuf)}
| "not"    	 		 {NOT :: (read lexbuf)}
| ('/' '\\' )    	 {AND :: (read lexbuf)}
| ('\\' '/')    	 {OR :: (read lexbuf)}
| "="    	 		 {EQ :: (read lexbuf)}
| ">"    	 		 {GTA :: (read lexbuf)}
| "<"    	 		 {LTA :: (read lexbuf)}
| ">="    	 		 {GEQ :: (read lexbuf)}
| "<="    	 		 {LEQ :: (read lexbuf)}
| "if"    	 		 {IF :: (read lexbuf)}
| "then"    	     {THEN :: (read lexbuf)}
| "else"    	 	 {ELSE :: (read lexbuf)}
| "def"    	 		 {DEF :: (read lexbuf)}
| ";"    	 		 {DELIMITER :: (read lexbuf)}
| identifier as s  	 {ID (s) :: (read lexbuf)}
| eof                {[]}
| whitespace         {(read lexbuf)}
| _ as c             {raise (Syntax_error(c))}




{
  let scanner s = read (Lexing.from_string s)
}




(*
Assumptions:
1. Whitespaces are not necessary.
2. an exception, 'Syntax_error' is defined. 




Testing:
1. scanner "52 - 5 ";;
- : token list = [INT 52; MINUS; INT 5]
2. # scanner "( 4 + 9 div rii) * -8;";;
- : token list = [LP; INT 4; PLUS; INT 9; DIV; ID "rii"; RP; MUL; INT (-8); DELIMITER]
3. scanner "not (T) /\\ (F) else def a = 56 div 22 (()";;
- : token list =
[NOT; LP; TRUE; RP; AND; LP; FALSE; RP; ELSE; DEF; ID "a"; EQ; INT 56; DIV;
 INT 22; LP; LP; RP]
4. scanner "-980 0 -332 5 a44TTvgT jjg55GG4dd3";;
- : token list =
[INT (-980); INT 0; INT (-332); INT 5; ID "a44TTvgT"; ID "jjg55GG4dd3"]
5. scanner "99 fjkf% DDf";;
Exception: Syntax_error '%'.
6. scanner "ifff then";;
- : token list = [ID "ifff"; THEN]
7. scanner "[ a + b*c]";;
Exception: Syntax_error '['.
8. scanner " g div B";;
Exception: Syntax_error 'B'.

Also tested on samples given on moodle : produced the expected result


*)