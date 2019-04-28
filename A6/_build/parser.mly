%{
    open Eval
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <char> ID
%token <char> FUNC
%token EQ LP RP COMMA LET RET PR VR SHOW EOF 
%start exp

%type <Eval.cmd> exp
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

/* The grammars written below are dummy. Please rewrite it as per the specifications. */



/* Implement the grammar rules for definitions, which may use the parser for expression  */

exp:
    exp_temp EOF                            {$1}
;


exp_temp:
    return_func                             {$1}
    |  call_func                            {$1}
    |  set_var                              {$1}
    |  show_exp                             {$1}
;


show_exp:
    SHOW PR                                  {Showpr}
    | SHOW VR                                {Showvr}
;

return_func:
    RET                                      { Ret }
;

call_func:
    FUNC LP constant COMMA constant RP                           { Call($1,$3,$5)}
;   

set_var:
    LET ID EQ constant                          { Let($2,$4) }
;

constant:
    ID                                          { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                                       { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
;



