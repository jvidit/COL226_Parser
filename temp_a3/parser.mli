type token =
  | LP
  | RP
  | PLUS
  | MUL
  | EOL
  | EOF
  | DIV
  | MOD
  | SUB
  | ABS
  | NEG
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | INT of (int)
  | ID of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expression.expr_tree
