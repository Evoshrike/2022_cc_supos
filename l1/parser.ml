type token =
  | INT of (int)
  | ADD
  | SUB
  | MUL
  | DIV
  | SEMICOLON
  | SKIP
  | GTEQ
  | ASSIGN
  | DEREF
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | VAR of (string)
  | LPAREN
  | RPAREN
  | BEGIN
  | END
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"

let get_loc = Parsing.symbol_start_pos 

# 33 "parser.ml"
let yytransl_const = [|
  258 (* ADD *);
  259 (* SUB *);
  260 (* MUL *);
  261 (* DIV *);
  262 (* SEMICOLON *);
  263 (* SKIP *);
  264 (* GTEQ *);
  265 (* ASSIGN *);
  266 (* DEREF *);
  267 (* TRUE *);
  268 (* FALSE *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* WHILE *);
  273 (* DO *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* BEGIN *);
  278 (* END *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  274 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\006\000\005\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\002\000\001\000\002\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\006\000\
\004\000\003\000\002\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\005\000\000\000\000\000\000\000\007\000\
\000\000\000\000\000\000\022\000\009\000\000\000\000\000\008\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\003\000\000\000\018\000\006\000\000\000\000\000\012\000\
\013\000\000\000\000\000\000\000\000\000\021\000\000\000\000\000"

let yydgoto = "\002\000\
\012\000\013\000\021\000\022\000\017\000\015\000"

let yysindex = "\255\255\
\003\255\000\000\000\000\000\000\243\254\003\255\003\255\000\000\
\003\255\003\255\037\255\000\000\000\000\083\000\253\254\000\000\
\000\000\086\255\065\255\046\255\093\255\241\254\243\254\000\000\
\003\255\003\255\003\255\003\255\003\255\000\000\003\255\003\255\
\003\255\000\000\003\255\000\000\000\000\254\254\254\254\000\000\
\000\000\254\254\254\254\072\255\100\255\000\000\003\255\100\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\242\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\011\000\000\000\
\000\000\032\000\042\000\000\000\052\000\000\000\000\000\062\000"

let yygindex = "\000\000\
\000\000\007\000\008\000\233\255\253\255\000\000"

let yytablesize = 347
let yytable = "\001\000\
\010\000\027\000\028\000\003\000\016\000\031\000\036\000\020\000\
\014\000\004\000\011\000\046\000\005\000\018\000\019\000\006\000\
\020\000\024\000\007\000\037\000\008\000\009\000\000\000\010\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\014\000\
\038\000\039\000\040\000\041\000\042\000\003\000\043\000\044\000\
\045\000\015\000\000\000\004\000\000\000\000\000\023\000\025\000\
\026\000\027\000\028\000\017\000\000\000\029\000\048\000\009\000\
\000\000\000\000\000\000\011\000\000\000\016\000\000\000\000\000\
\000\000\034\000\025\000\026\000\027\000\028\000\000\000\000\000\
\029\000\025\000\026\000\027\000\028\000\000\000\000\000\029\000\
\000\000\033\000\030\000\000\000\000\000\000\000\047\000\025\000\
\026\000\027\000\028\000\000\000\000\000\029\000\025\000\026\000\
\027\000\028\000\035\000\032\000\029\000\025\000\026\000\027\000\
\028\000\000\000\000\000\029\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\010\000\010\000\000\000\000\000\010\000\000\000\
\010\000\000\000\000\000\000\000\011\000\011\000\010\000\010\000\
\011\000\010\000\011\000\000\000\010\000\000\000\010\000\000\000\
\011\000\011\000\000\000\011\000\000\000\000\000\011\000\000\000\
\011\000\014\000\014\000\000\000\000\000\014\000\000\000\014\000\
\000\000\000\000\000\000\015\000\015\000\014\000\014\000\015\000\
\014\000\015\000\000\000\014\000\000\000\014\000\000\000\015\000\
\015\000\017\000\015\000\000\000\000\000\015\000\000\000\015\000\
\000\000\017\000\017\000\016\000\017\000\000\000\000\000\017\000\
\000\000\017\000\000\000\016\000\016\000\000\000\016\000\000\000\
\000\000\016\000\000\000\016\000\025\000\026\000\027\000\028\000\
\000\000\000\000\029\000"

let yycheck = "\001\000\
\000\000\004\001\005\001\001\001\018\001\009\001\022\001\022\001\
\001\000\007\001\000\000\035\000\010\001\006\000\007\000\013\001\
\009\000\011\000\016\001\023\000\018\001\019\001\255\255\021\001\
\255\255\023\001\255\255\255\255\255\255\255\255\255\255\000\000\
\025\000\026\000\027\000\028\000\029\000\001\001\031\000\032\000\
\033\000\000\000\255\255\007\001\255\255\255\255\010\001\002\001\
\003\001\004\001\005\001\000\000\255\255\008\001\047\000\019\001\
\255\255\255\255\255\255\023\001\255\255\000\000\255\255\255\255\
\255\255\020\001\002\001\003\001\004\001\005\001\255\255\255\255\
\008\001\002\001\003\001\004\001\005\001\255\255\255\255\008\001\
\255\255\017\001\000\000\255\255\255\255\255\255\015\001\002\001\
\003\001\004\001\005\001\255\255\255\255\008\001\002\001\003\001\
\004\001\005\001\006\001\014\001\008\001\002\001\003\001\004\001\
\005\001\255\255\255\255\008\001\255\255\255\255\255\255\255\255\
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
\255\255\255\255\002\001\003\001\255\255\255\255\006\001\255\255\
\008\001\255\255\255\255\255\255\002\001\003\001\014\001\015\001\
\006\001\017\001\008\001\255\255\020\001\255\255\022\001\255\255\
\014\001\015\001\255\255\017\001\255\255\255\255\020\001\255\255\
\022\001\002\001\003\001\255\255\255\255\006\001\255\255\008\001\
\255\255\255\255\255\255\002\001\003\001\014\001\015\001\006\001\
\017\001\008\001\255\255\020\001\255\255\022\001\255\255\014\001\
\015\001\006\001\017\001\255\255\255\255\020\001\255\255\022\001\
\255\255\014\001\015\001\006\001\017\001\255\255\255\255\020\001\
\255\255\022\001\255\255\014\001\015\001\255\255\017\001\255\255\
\255\255\020\001\255\255\022\001\002\001\003\001\004\001\005\001\
\255\255\255\255\008\001"

let yynames_const = "\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  SEMICOLON\000\
  SKIP\000\
  GTEQ\000\
  ASSIGN\000\
  DEREF\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  LPAREN\000\
  RPAREN\000\
  BEGIN\000\
  END\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 29 "parser.mly"
                         ( _1 )
# 232 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 32 "parser.mly"
                                     ( Past.Integer (get_loc(), _1) )
# 239 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 33 "parser.mly"
                                     ( _2 )
# 246 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 34 "parser.mly"
                                         ( Past.UnaryOp(get_loc(), Past.NEG, _2) )
# 253 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                                     ( Past.Skip(get_loc()) )
# 259 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 36 "parser.mly"
                     ( Past.UnaryOp(get_loc(),Past.DEREF _2) )
# 266 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                                     ( Past.Var (get_loc(), _1) )
# 273 "parser.ml"
               : 'loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
                                     ( Past.Var (get_loc(), _1) )
# 280 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 45 "parser.mly"
                                     (  _1 )
# 287 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 46 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.ADD, _3) )
# 295 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 47 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.SUB, _3) )
# 303 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 48 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.MUL, _3) )
# 311 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 49 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.DIV, _3) )
# 319 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 50 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.GTEQ, _3) )
# 327 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 51 "parser.mly"
                                     ( Past.Op(get_loc(), _1, Past.ASSIGN, _3) )
# 335 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 52 "parser.mly"
                                     ( Past.If(get_loc(), _2, _4, _6) )
# 344 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 53 "parser.mly"
                                     ( Past.While(get_loc(), _2, _4) )
# 352 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr list) in
    Obj.repr(
# 54 "parser.mly"
                                     ( Past.Seq(get_loc(), _2) )
# 359 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 55 "parser.mly"
                     ( Past.UnaryOp(get_loc(),Past.DEREF _2) )
# 366 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 59 "parser.mly"
                                     ( [_1] )
# 373 "parser.ml"
               : Past.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr list) in
    Obj.repr(
# 60 "parser.mly"
                                     ( _1 :: _3  )
# 381 "parser.ml"
               : Past.expr list))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Past.expr)
