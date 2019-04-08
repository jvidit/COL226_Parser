%{
    open A1
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
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    disj_expression EOF                          { $1 }   
    | disj_expression                            { $1 }
;

disj_expression:
    disj_expression DISJ conj_expression         { Disjunction($1,$3) }
    |  conj_expression                           { $1 }
;

conj_expression:
    conj_expression CONJ not_expression        { Conjunction($1,$3) }
    |  not_expression                          { $1 }
;


not_expression:
    NOT compare_expression                         { Not($2)}
    | compare_expression                           { $1 }
;

compare_expression:
    compare_expression EQ minus_expression          { Equals($1,$3)}
    | compare_expression LT minus_expression        { LessT($1,$3) }
    | compare_expression GT minus_expression        { GreaterT($1,$3)}
    | compare_expression LT EQ minus_expression     { LessTE($1,$4)}
    | compare_expression EQ LT minus_expression     { LessTE($1,$4)}
    | compare_expression GT EQ minus_expression     { GreaterTE($1,$4)}
    | compare_expression EQ GT minus_expression     { GreaterTE($1,$4)}
    | minus_expression                              { $1 }
;

minus_expression:
    minus_expression MINUS rem_expression       { Sub($1,$3) }
    | minus_expression PLUS rem_expression      { Add($1,$3) }
    | rem_expression                            { $1 }
;

rem_expression:
    rem_expression REM mult_expression         { Rem($1,$3) }
    | mult_expression                            { $1 }
;

mult_expression:
    mult_expression TIMES div_expression        { Mult($1,$3) }
    | div_expression                            { $1 }
;

div_expression:
    div_expression DIV abs_expression           { Div($1,$3) }
    | abs_expression                            { $1 }
;

abs_expression:
    ABS neg_expression                          { Abs($2)}
    | neg_expression                            { $1 }
;

neg_expression:
    TILDA neg_expression                       { Negative($2)}
    | ifte_expression                          { $1 }
;

ifte_expression:
    IF main THEN main ELSE main FI             { IfThenElse($2,$4,$6)}
    | proj_expression                          { $1 }
;

proj_expression:
    PROJ LP INT COMMA INT RP ifte_expression   { Project(($3,$5) , $7) }
    | tuple_expression                          { $1 }
;

tuple_expression:                              
    LP RP                                       {Tuple(0,[])}
    | LP comma_expression RP                    {Tuple(List.length ($2),$2)}
    | constant                                  { $1 }
;

comma_expression:                                                           /*cannot have tupes of size 1, due to confusion with Parenthesis*/
    main COMMA main                             {$1::[$3] }
    | comma_expression COMMA main               {$1@[$3]}
;

constant:
    ID                                          { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                                       { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
    | BOOL                                      { B($1)}
    | LP main RP                                { InParen($2) }
;







