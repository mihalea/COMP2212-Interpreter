type token =
  | INT of (int)
  | IDENT of (string)
  | LITERAL of (string)
  | BEGIN
  | END
  | PRINT
  | VAR_DEC
  | CONCAT
  | UNION
  | INTERSECT
  | DIFF
  | SEMICOL
  | COMMA
  | ADD
  | EOF
  | EOL
  | FOR
  | TO
  | IN
  | LCURLY
  | RCURLY
  | EQUALS
  | PLUS

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open ParseTree
# 31 "parser.ml"
let yytransl_const = [|
  260 (* BEGIN *);
  261 (* END *);
  262 (* PRINT *);
  263 (* VAR_DEC *);
  264 (* CONCAT *);
  265 (* UNION *);
  266 (* INTERSECT *);
  267 (* DIFF *);
  268 (* SEMICOL *);
  269 (* COMMA *);
  270 (* ADD *);
    0 (* EOF *);
  271 (* EOL *);
  272 (* FOR *);
  273 (* TO *);
  274 (* IN *);
  275 (* LCURLY *);
  276 (* RCURLY *);
  277 (* EQUALS *);
  278 (* PLUS *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
  259 (* LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\004\000\002\000\002\000\005\000\005\000\005\000\
\005\000\006\000\007\000\007\000\007\000\008\000\008\000\009\000\
\009\000\009\000\010\000\010\000\010\000\011\000\011\000\011\000\
\013\000\013\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\002\000\003\000\001\000\001\000\001\000\
\001\000\004\000\001\000\001\000\001\000\003\000\004\000\002\000\
\007\000\007\000\001\000\001\000\003\000\001\000\001\000\003\000\
\001\000\003\000\001\000\002\000\003\000\003\000\003\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\019\000\002\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\000\000\006\000\
\007\000\008\000\009\000\000\000\000\000\000\000\000\000\016\000\
\000\000\000\000\000\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\001\000\000\000\023\000\000\000\
\014\000\005\000\020\000\021\000\024\000\000\000\030\000\031\000\
\032\000\010\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\000\000\018\000\017\000"

let yydgoto = "\002\000\
\004\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\021\000\022\000\029\000"

let yysindex = "\011\000\
\016\255\000\000\002\255\000\000\000\000\000\000\000\000\038\255\
\051\255\051\255\003\255\050\255\053\255\000\000\044\255\000\000\
\000\000\000\000\000\000\064\255\052\255\036\255\059\255\000\000\
\066\255\063\255\062\255\000\000\068\255\089\000\069\255\080\255\
\038\255\002\255\083\255\080\255\024\255\024\255\024\255\038\255\
\083\255\051\255\088\255\000\000\000\000\080\255\000\000\052\255\
\000\000\000\000\000\000\000\000\000\000\059\255\000\000\000\000\
\000\000\000\000\000\255\073\255\000\000\052\255\002\255\002\255\
\074\255\075\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\255\000\000\000\000\000\000\
\000\000\000\000\000\000\081\255\084\255\085\255\040\255\000\000\
\000\000\000\000\078\255\000\000\000\000\000\000\000\000\000\000\
\000\000\039\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\255\
\000\000\000\000\000\000\000\000\000\000\060\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\087\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\229\255\248\255\000\000\000\000\000\000\002\000\000\000\
\000\000\232\255\022\000\040\000\057\000"

let yytablesize = 100
let yytable = "\023\000\
\025\000\026\000\005\000\006\000\007\000\027\000\050\000\008\000\
\009\000\024\000\052\000\001\000\033\000\033\000\033\000\033\000\
\059\000\010\000\063\000\003\000\011\000\035\000\028\000\047\000\
\023\000\006\000\051\000\047\000\054\000\054\000\054\000\023\000\
\051\000\060\000\049\000\065\000\066\000\047\000\005\000\006\000\
\007\000\058\000\011\000\004\000\037\000\038\000\039\000\023\000\
\027\000\027\000\027\000\020\000\006\000\048\000\030\000\034\000\
\011\000\053\000\004\000\036\000\031\000\020\000\027\000\027\000\
\027\000\020\000\032\000\062\000\027\000\027\000\027\000\027\000\
\032\000\033\000\043\000\020\000\055\000\056\000\057\000\041\000\
\042\000\006\000\007\000\005\000\006\000\035\000\040\000\044\000\
\045\000\046\000\027\000\064\000\011\000\067\000\068\000\012\000\
\013\000\025\000\015\000\061\000"

let yycheck = "\008\000\
\009\000\010\000\001\001\002\001\003\001\003\001\034\000\006\001\
\007\001\008\000\035\000\001\000\009\001\010\001\011\001\012\001\
\041\000\016\001\019\001\004\001\019\001\022\001\020\001\032\000\
\033\000\002\001\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\033\000\063\000\064\000\046\000\001\001\002\001\
\003\001\040\000\019\001\005\001\009\001\010\001\011\001\008\001\
\009\001\010\001\011\001\012\001\002\001\032\000\005\001\012\001\
\019\001\036\000\020\001\008\001\008\001\022\001\009\001\010\001\
\011\001\012\001\014\001\046\000\009\001\010\001\011\001\012\001\
\014\001\021\001\013\001\022\001\037\000\038\000\039\000\017\001\
\018\001\002\001\003\001\001\001\002\001\022\001\021\001\020\001\
\000\000\021\001\003\001\019\001\012\001\020\001\020\001\012\001\
\012\001\020\001\012\001\043\000"

let yynames_const = "\
  BEGIN\000\
  END\000\
  PRINT\000\
  VAR_DEC\000\
  CONCAT\000\
  UNION\000\
  INTERSECT\000\
  DIFF\000\
  SEMICOL\000\
  COMMA\000\
  ADD\000\
  EOF\000\
  EOL\000\
  FOR\000\
  TO\000\
  IN\000\
  LCURLY\000\
  RCURLY\000\
  EQUALS\000\
  PLUS\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'statements) in
    Obj.repr(
# 28 "parser.mly"
                             (_2)
# 183 "parser.ml"
               : ParseTree.tTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "parser.mly"
          (TermVar(_1))
# 190 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parser.mly"
            (TermString(_1))
# 197 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 39 "parser.mly"
                      (_1)
# 204 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 40 "parser.mly"
                                 ( MultiStatement (_1, _3) )
# 212 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec_op) in
    Obj.repr(
# 44 "parser.mly"
           ( _1 )
# 219 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 45 "parser.mly"
              ( _1 )
# 226 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mut_op) in
    Obj.repr(
# 46 "parser.mly"
           ( _1 )
# 233 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exec_op) in
    Obj.repr(
# 47 "parser.mly"
            ( _1 )
# 240 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 51 "parser.mly"
                                    ( Declaration (_2, _4) )
# 248 "parser.ml"
               : 'dec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 55 "parser.mly"
                  (_1)
# 255 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 56 "parser.mly"
                  (_1)
# 262 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 57 "parser.mly"
                  (_1)
# 269 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 61 "parser.mly"
                           ( TermMut (_1, _3) )
# 277 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 62 "parser.mly"
                                      ( TermMut(_1, TermConcat (_1, _4)) )
# 285 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 66 "parser.mly"
                    ( PrintOperation (_2))
# 292 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 67 "parser.mly"
                                                ( ForOperation (_2, _4, _6))
# 301 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'int_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 68 "parser.mly"
                                                        ( ForLoop ( _2, _4, _6))
# 310 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 72 "parser.mly"
        (TermInteger(_1))
# 317 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 73 "parser.mly"
          (_1)
# 324 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 74 "parser.mly"
                                     (TermPlus(_1,_3))
# 332 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 78 "parser.mly"
            (_1)
# 339 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 79 "parser.mly"
            (_1)
# 346 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 80 "parser.mly"
                                       (TermConcat (_1, _3) )
# 354 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                (_1 :: [] )
# 361 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 85 "parser.mly"
                           ( _1 :: _3 )
# 369 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 89 "parser.mly"
          ( _1 )
# 376 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                  ( TermArgs([]) )
# 382 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 91 "parser.mly"
                       (TermArgs( _2 ) )
# 389 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 92 "parser.mly"
                                      ( TermUnion (_1, _3) )
# 397 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 93 "parser.mly"
                                          ( TermIntersection ( _1, _3 ) )
# 405 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 94 "parser.mly"
                                     ( TermDifference ( _1, _3 ) )
# 413 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 95 "parser.mly"
                            ( TermAdd (_1, _3) )
# 421 "parser.ml"
               : 'set_operation))
(* Entry start *)
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
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ParseTree.tTerm)
