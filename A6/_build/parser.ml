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
  | SCOPE
  | SETUP
  | STK
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Eval
# 24 "parser.ml"
let yytransl_const = [|
  260 (* EQ *);
  261 (* LP *);
  262 (* RP *);
  263 (* COMMA *);
  264 (* LET *);
  265 (* RET *);
  266 (* PR *);
  267 (* VR *);
  268 (* SHOW *);
  269 (* SCOPE *);
  270 (* SETUP *);
  271 (* STK *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* ID *);
  259 (* FUNC *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\005\000\005\000\
\005\000\003\000\004\000\006\000\006\000\007\000\007\000\008\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\002\000\006\000\004\000\001\000\001\000\001\000\002\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\006\000\017\000\
\000\000\003\000\004\000\005\000\000\000\014\000\000\000\000\000\
\007\000\008\000\009\000\001\000\016\000\000\000\012\000\013\000\
\015\000\000\000\000\000\011\000\000\000\010\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\012\000\022\000\023\000\024\000"

let yysindex = "\005\000\
\001\255\000\000\006\255\003\255\000\000\248\254\000\000\000\000\
\008\000\000\000\000\000\000\000\255\254\000\000\012\255\015\255\
\000\000\000\000\000\000\000\000\000\000\013\255\000\000\000\000\
\000\000\255\254\255\254\000\000\016\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\247\255\017\000\000\000"

let yytablesize = 22
let yytable = "\021\000\
\014\000\017\000\018\000\003\000\014\000\001\000\019\000\020\000\
\004\000\005\000\013\000\015\000\006\000\025\000\007\000\015\000\
\028\000\029\000\026\000\027\000\016\000\030\000"

let yycheck = "\001\001\
\002\001\010\001\011\001\003\001\002\001\001\000\015\001\000\000\
\008\001\009\001\005\001\013\001\012\001\002\001\014\001\013\001\
\026\000\027\000\004\001\007\001\004\000\006\001"

let yynames_const = "\
  EQ\000\
  LP\000\
  RP\000\
  COMMA\000\
  LET\000\
  RET\000\
  PR\000\
  VR\000\
  SHOW\000\
  SCOPE\000\
  SETUP\000\
  STK\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  FUNC\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp_temp) in
    Obj.repr(
# 33 "parser.mly"
                                            (_1)
# 121 "parser.ml"
               : Eval.cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                                            (Ret)
# 127 "parser.ml"
               : 'exp_temp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'call_func) in
    Obj.repr(
# 39 "parser.mly"
                                            (_1)
# 134 "parser.ml"
               : 'exp_temp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_var) in
    Obj.repr(
# 40 "parser.mly"
                                            (_1)
# 141 "parser.ml"
               : 'exp_temp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'show_exp) in
    Obj.repr(
# 41 "parser.mly"
                                            (_1)
# 148 "parser.ml"
               : 'exp_temp))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                                            (Setup)
# 154 "parser.ml"
               : 'exp_temp))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                             (Showpr)
# 160 "parser.ml"
               : 'show_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                                             (Showvr)
# 166 "parser.ml"
               : 'show_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                                             (Showstk)
# 172 "parser.ml"
               : 'show_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : char) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'constant) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'constant) in
    Obj.repr(
# 54 "parser.mly"
                                                                 ( Call(_1,_3,_5))
# 181 "parser.ml"
               : 'call_func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'id_constant) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 59 "parser.mly"
                                                         ( Let(_2,_4) )
# 189 "parser.ml"
               : 'set_var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'id_constant) in
    Obj.repr(
# 63 "parser.mly"
                                                        (_1)
# 196 "parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'num_constant) in
    Obj.repr(
# 64 "parser.mly"
                                                        (_1)
# 203 "parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 68 "parser.mly"
                                                ( Var(0,_1) )
# 210 "parser.ml"
               : 'id_constant))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 69 "parser.mly"
                                                ( Var(1,_2) )
# 217 "parser.ml"
               : 'id_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "parser.mly"
                                              ( N(_1) )
# 224 "parser.ml"
               : 'num_constant))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Eval.cmd)
