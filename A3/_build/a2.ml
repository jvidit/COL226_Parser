# 1 "a2.mll"
 
  open A3
  exception Not_implemented
  exception Syntax_Error

# 8 "a2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\228\255\229\255\230\255\079\000\000\000\233\255\001\000\
    \000\000\000\000\001\000\238\255\239\255\240\255\241\255\242\255\
    \011\000\001\000\000\000\002\000\247\255\248\255\249\255\002\000\
    \251\255\000\000\163\000\247\000\255\255\066\001\000\000\252\255\
    \001\000\250\255\001\000\246\255\020\000\245\255\244\255\243\255\
    \237\255\020\000\012\000\236\255\008\000\023\000\235\255\234\255\
    \014\000\031\000\232\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\024\000\027\000\255\255\027\000\
    \027\000\027\000\027\000\255\255\255\255\255\255\255\255\255\255\
    \027\000\027\000\027\000\027\000\255\255\255\255\255\255\027\000\
    \255\255\027\000\002\000\001\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\255\255\255\255\000\000\000\000\
    \255\255\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \012\000\011\000\020\000\022\000\006\000\021\000\000\000\017\000\
    \028\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\039\000\000\000\013\000\015\000\014\000\000\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\026\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\027\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\000\000\016\000\038\000\000\000\000\000\
    \000\000\025\000\030\000\000\000\019\000\008\000\007\000\040\000\
    \041\000\010\000\047\000\034\000\044\000\018\000\023\000\036\000\
    \005\000\032\000\048\000\031\000\009\000\033\000\004\000\035\000\
    \037\000\042\000\043\000\045\000\046\000\049\000\024\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\000\000\000\000\000\000\000\000\004\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\000\000\000\000\
    \002\000\000\000\004\000\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\000\000\000\000\000\000\000\000\004\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\016\000\255\255\000\000\000\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\017\000\255\255\255\255\
    \255\255\000\000\025\000\255\255\000\000\000\000\000\000\010\000\
    \009\000\000\000\007\000\019\000\008\000\000\000\000\000\018\000\
    \000\000\023\000\005\000\030\000\000\000\032\000\004\000\034\000\
    \036\000\041\000\042\000\044\000\045\000\048\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\049\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\255\255\255\255\255\255\255\255\004\000\255\255\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\026\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\255\255\255\255\
    \000\000\255\255\026\000\255\255\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\027\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\255\255\255\255\255\255\255\255\027\000\255\255\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read lexbuf =
   __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 27 "a2.mll"
              n
# 204 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 27 "a2.mll"
                      (INT (int_of_string n) )
# 208 "a2.ml"

  | 1 ->
# 28 "a2.mll"
                      (BOOL(true))
# 213 "a2.ml"

  | 2 ->
# 29 "a2.mll"
                      (BOOL(false))
# 218 "a2.ml"

  | 3 ->
# 30 "a2.mll"
                      (ABS)
# 223 "a2.ml"

  | 4 ->
# 31 "a2.mll"
                      (TILDA)
# 228 "a2.ml"

  | 5 ->
# 32 "a2.mll"
                      (NOT)
# 233 "a2.ml"

  | 6 ->
# 33 "a2.mll"
                      (PLUS)
# 238 "a2.ml"

  | 7 ->
# 34 "a2.mll"
                      (MINUS)
# 243 "a2.ml"

  | 8 ->
# 35 "a2.mll"
                      (TIMES)
# 248 "a2.ml"

  | 9 ->
# 36 "a2.mll"
                      (DIV)
# 253 "a2.ml"

  | 10 ->
# 37 "a2.mll"
                      (REM)
# 258 "a2.ml"

  | 11 ->
# 38 "a2.mll"
                      (CONJ)
# 263 "a2.ml"

  | 12 ->
# 39 "a2.mll"
                      (DISJ)
# 268 "a2.ml"

  | 13 ->
# 40 "a2.mll"
                      (EQ)
# 273 "a2.ml"

  | 14 ->
# 41 "a2.mll"
                      (GT)
# 278 "a2.ml"

  | 15 ->
# 42 "a2.mll"
                      (LT)
# 283 "a2.ml"

  | 16 ->
# 43 "a2.mll"
                      (LP)
# 288 "a2.ml"

  | 17 ->
# 44 "a2.mll"
                      (RP)
# 293 "a2.ml"

  | 18 ->
# 45 "a2.mll"
                      (IF)
# 298 "a2.ml"

  | 19 ->
# 46 "a2.mll"
                      (THEN)
# 303 "a2.ml"

  | 20 ->
# 47 "a2.mll"
                      (ELSE)
# 308 "a2.ml"

  | 21 ->
# 48 "a2.mll"
                      (FI)
# 313 "a2.ml"

  | 22 ->
# 49 "a2.mll"
                      (COMMA)
# 318 "a2.ml"

  | 23 ->
# 50 "a2.mll"
                      (PROJ)
# 323 "a2.ml"

  | 24 ->
let
# 51 "a2.mll"
                   s
# 329 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 51 "a2.mll"
                      (ID(s))
# 333 "a2.ml"

  | 25 ->
# 52 "a2.mll"
                      (read lexbuf)
# 338 "a2.ml"

  | 26 ->
# 53 "a2.mll"
                      (EOF)
# 343 "a2.ml"

  | 27 ->
# 54 "a2.mll"
                      (raise Syntax_Error)
# 348 "a2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;
