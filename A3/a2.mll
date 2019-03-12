{
  open A3
  exception Not_implemented
  exception Syntax_Error
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a3.ml)
*)


(*regex used*)
let whitespace=[' ' '\t']
let digit = ['0'-'9']                   (*regex for any number with digits 0-9*)
let integer = ['-']? (['1'-'9']digit* | ['0'])      (*either integer is 0, or a integer with no leading zeros. Sign is optional*)
let lower_letter = ['a'-'z']                (*any string with lower case letters*)
let upper_letter = ['A'-'Z']
let letter = ['a'-'z' 'A'-'Z']                (*any string with letters of both cases*)                           

let identifier = upper_letter (letter | digit | '\'' | '_')*       (*any string beginning with lower case letter followed by alphanumeric*)

rule read = parse
   integer as n       {INT (int_of_string n) }
   | 'T'              {BOOL(true)}
   | 'F'              {BOOL(false)}
   | "abs"            {ABS}
   | '~'              {TILDA}
   | identifier as s  {ID(s)}
   | "not"            {NOT}
   | '+'              {PLUS}
   | '-'              {MINUS}
   | '*'              {TIMES}
   | "div"            {DIV}
   | "mod"            {REM}
   | ('/' '\\' )      {CONJ}
   | ('\\' '/')       {DISJ}
   | '='              {EQ}
   | '>'              {GT}
   | '<'              {LT}
   | '('              {LP}
   | ')'              {RP}
   | "if"             {IF}
   | "then"           {THEN}
   | "else"           {ELSE}
   | "fi"             {FI}
   | ','              {COMMA}
   | "proj"           {PROJ}
   | whitespace       {read lexbuf}
   | eof              {EOF}
   | _                {raise Syntax_Error}


