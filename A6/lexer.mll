{
  open Parser
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
let integer = (['1'-'9']digit* | ['0'])      (*either integer is 0, or a integer with no leading zeros. Sign is optional*)

let upper_letter = ['A'-'Z']
let lower_letter = ['a'-'z']
                        
rule read = parse
   integer as n       {INT (int_of_string n) }
   
   | '='              {EQ}
   
   | '('              {LP}
   | ')'              {RP}
   
   | ','              {COMMA}

   | '~'              {SCOPE}
   
   | "let"            {LET}
   | "ret"            {RET}
   | "show"           {SHOW}
   | "procedures"     {PR}
   | "variables"      {VR}
   | "stack"          {STK}
   | "setup"          {SETUP}

   | upper_letter as c  {FUNC(c)}
   | lower_letter as c  {ID(c)}
   | whitespace       {read lexbuf}
   | eof              {EOF}
   | _                {raise Syntax_Error}



