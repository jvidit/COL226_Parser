/* File parser.mly */
%{
    open Expression
    exception EOF
%}

%token LP RP PLUS MUL EOL EOF DIV MOD SUB ABS NEG TRUE FALSE AND OR NOT
%token <int> INT
%token <string> ID
%start main             /* the entry point */
%type <Expression.expr_tree> main                     /* Specifying the type to be returned for the grammar symbol main */
%%
main:
    add_expression EOL                 { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    | EOF                              { NULL }
;



/* arithmetic functions */
add_expression:
      or_expression                      { OR($1,$3)  }
    | add_expression PLUS sub_expression { PLUS($1,$3) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | sub_expression                  { $1 }
;

sub_expression:
    sub_expression SUB mult_expression { SUB($1,$3) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | mult_expression                  { $1 }
;

mult_expression:
    mult_expression MUL div_expression      { INTO($1,$3) }
    | div_expression                         { $1 }
;

div_expression:
    div_expression DIV mod_expression      { DIV($1,$3) }
    | mod_expression                        { $1 }
;

mod_expression:
    mod_expression MOD constant         { MOD($1,$3) }
    | abs_expression                        { $1 }
;

abs_expression:
    ABS neg_expression                  { ABS($2) }
    | neg_expression                       { $1 }
;

neg_expression:
    NEG constant                         { NEG($2) }
    | constant                             { $1 }
;

constant:
    ID                                 { VAR($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { NUM($1) }      /* To be interpreted as an integer with its value as tokenised   */
    | LP add_expression RP             { ARITH_PARENTHESIS($2)     }
;

/*boolean*/

or_expression:
    or_expression OR and_expression      { OR($1,$3) }
    | and_expression                         { $1 }
;

and_expression:
    and_expression AND not_expression      { AND($1,$3) }
    | not_expression                         { $1 }
;

not_expression:
    NOT bool_constant                  { NOT($2) }
    | bool_constant                    { $1 }

bool_constant:
    ID                                 { VAR($1) }      /* To be interpreted as a variable name with string as tokenised */
    | T                                { TRUE }     
    | F                                { FALSE } 
    | LP and_expression RP             { BOOL_PARENTHESIS($2)     }
;







/*TODO
 * Add support in the grammar for parenthesis
 *  - Adding the parenthesis should be able to change the parse tree to effectively modify precedence.
 *  E.g. 1+2*3  ==>        PLUS
 *                        /    \
 *                      NUM1   INTO
 *                            /    \
 *                         NUM 2  NUM 3
 *
 *  vs (1+2)*3  ==>        INTO
 *                        /    \
 *                     PLUS     NUM 3
 *                    /    \
 *                 NUM 1   NUM 2
 *
 * Try completing the calculator for basic arithmetic by adding division and subtraction, while respecting precedence
 * This will require changes right from the lexer.mll and parser.mly to the definition of print and evaluation functions in expression.ml
 *
 * ADVANCED
 * Try creating an expression for assigning new variables in the variable_set in the expression.ml file, so that they can be reused in a later evaluation statement.
 * E.g. myVar:=4.
 *      // Stores the integer value 4 corresponding to the string myVar in variable_set
 *
 *      myVar*3+1
 *      Answer: 13
 * */
