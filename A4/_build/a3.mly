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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ 
LET IN END BACKSLASH DOT DEF SEMICOLON COLON TINT TBOOL TUNIT PARALLEL LOCAL EOF
%start def_parser exp_parser type_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%type <A1.exptype> type_parser
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

/* The grammars written below are dummy. Please rewrite it as per the specifications. */



/* Implement the grammar rules for definitions, which may use the parser for expression  */


def_parser:
    def_parser_temp EOF       {$1}
;

def_parser_temp:
    parallel_list                  {Parallel($1)}
    |   seq_list                   {Sequence($1)}
    |   local_def                  { $1 } 
;   

parallel_list:
    seq_list PARALLEL local_def          { [Sequence($1)]@[$3]}
    | parallel_list PARALLEL local_def   { $1@[$3] }
    | local_def PARALLEL local_def       { $1::[$3]}
;

seq_list:
    parallel_list SEMICOLON local_def          { [Parallel($1)]@[$3]}
    | seq_list SEMICOLON local_def            { $1@[$3] }
    | local_def SEMICOLON local_def            { $1::[$3]}
;

local_def:
    LOCAL def_parser_temp IN def_parser_temp END  {Local($2,$4)}
    |  simple_def                       { $1 }
;

simple_def:
    DEF ID COLON type_parser EQ disj_expression                {Simple($2,$4,$6)}
;




type_parser:
    func_type_parser                                         {$1}
    | base_type_parser                                       {$1}
;

func_type_parser:
    func_type_parser MINUS GT base_type_parser               {Tfunc($1,$4)}
    | base_type_parser MINUS GT base_type_parser             {Tfunc($1,$4)}
;

typevar_list:
    base_type_parser TIMES base_type_parser                  {[$1]@[$3]}
    |  typevar_list TIMES base_type_parser                   {$1@[$3]}
;

base_type_parser:
    TBOOL                                           {Tbool}
    | TUNIT                                         {Tunit}
    | TINT                                          {Tint}
    | LP typevar_list RP                            {Ttuple($2)}
    | LP func_type_parser RP                        {$2}
;



exp_parser:          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    disj_expression EOF                          { $1 }   
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
    mult_expression TIMES abs_expression        { Mult($1,$3) }
    | mult_expression DIV abs_expression        { Div($1,$3)  }
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
    IF disj_expression THEN disj_expression ELSE disj_expression FI             { IfThenElse($2,$4,$6)}
    | proj_expression                          { $1 }
;

proj_expression:
    PROJ LP INT COMMA INT RP ifte_expression   { Project(($3,$5) , $7) }
    | tuple_expression                          { $1 }
;

tuple_expression:                              
    LP RP                                       {Tuple(0,[])}
    | LP comma_expression RP                    {Tuple(List.length ($2),$2)}
    | f_call_expression                         { $1 }
;

comma_expression:                                                           /*cannot have tupes of size 1, due to confusion with Parenthesis*/
    disj_expression COMMA disj_expression                             {$1::[$3]}
    | comma_expression COMMA disj_expression                   {$1@[$3]}
;

f_call_expression:
    f_abstract_expression LP ifte_expression RP               {FunctionCall($1,$3)}
    |   f_abstract_expression                               {$1}
;

f_abstract_expression:  
    BACKSLASH ID COLON type_parser DOT let_expression                  {FunctionAbstraction($2,$4,$6)}
    |   let_expression                                      {$1}
;

let_expression:
    LET def_parser_temp IN disj_expression END              {Let($2,$4)}
    | constant                                              {$1}
;

constant:
    ID                                          { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                                       { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
    | BOOL                                      { B($1)}
    | LP disj_expression RP                     { InParen($2) }
;



