type token =
  | INT of (int)
  | ID of (char)
  | FUNC of (char)
  | EQ
  | LP
  | RP
  | COMMA
  | LET
  | RET
  | PR
  | VR
  | SHOW
  | EOF

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Eval.cmd