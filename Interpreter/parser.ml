type token =
  | INT of (int)
  | IDENT of (string)
  | LITERAL of (string)
  | TRUE of (bool)
  | FALSE of (bool)
  | BEGIN
  | END
  | PRINT
  | VAR_DEC
  | CONCAT
  | LT
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
  | IF
  | ELSE
  | LCURLY
  | RCURLY
  | LPAREN
  | RPAREN
  | EQUALS
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open ParseTree
# 42 "parser.ml"
let yytransl_const = [|
  262 (* BEGIN *);
  263 (* END *);
  264 (* PRINT *);
  265 (* VAR_DEC *);
  266 (* CONCAT *);
  267 (* LT *);
  268 (* UNION *);
  269 (* INTERSECT *);
  270 (* DIFF *);
  271 (* SEMICOL *);
  272 (* COMMA *);
  273 (* ADD *);
    0 (* EOF *);
  274 (* EOL *);
  275 (* FOR *);
  276 (* TO *);
  277 (* IN *);
  278 (* IF *);
  279 (* ELSE *);
  280 (* LCURLY *);
  281 (* RCURLY *);
  282 (* LPAREN *);
  283 (* RPAREN *);
  284 (* EQUALS *);
  285 (* PLUS *);
  286 (* MINUS *);
  287 (* TIMES *);
  288 (* DIV *);
  289 (* MOD *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
  259 (* LITERAL *);
  260 (* TRUE *);
  261 (* FALSE *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\004\000\005\000\005\000\002\000\002\000\006\000\
\006\000\006\000\006\000\007\000\008\000\008\000\008\000\008\000\
\009\000\009\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\012\000\014\000\014\000\014\000\015\000\015\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\001\000\001\000\002\000\003\000\001\000\
\001\000\001\000\001\000\004\000\001\000\001\000\001\000\001\000\
\003\000\004\000\002\000\007\000\007\000\007\000\011\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\001\000\001\000\
\003\000\001\000\001\000\003\000\001\000\003\000\001\000\002\000\
\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\046\000\024\000\002\000\003\000\004\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\031\000\034\000\000\000\008\000\009\000\010\000\011\000\000\000\
\000\000\000\000\016\000\000\000\019\000\000\000\000\000\000\000\
\000\000\040\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\001\000\000\000\032\000\000\000\017\000\007\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\000\000\042\000\043\000\044\000\012\000\000\000\000\000\000\000\
\038\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\020\000\000\000\000\000\000\000\000\000\023\000"

let yydgoto = "\002\000\
\004\000\015\000\065\000\017\000\018\000\019\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\035\000"

let yysindex = "\013\000\
\026\255\000\000\019\255\000\000\000\000\000\000\000\000\000\000\
\000\000\001\255\047\255\047\255\053\255\008\255\098\255\099\255\
\000\000\000\000\092\255\000\000\000\000\000\000\000\000\067\255\
\121\255\058\255\000\000\116\255\000\000\106\255\031\255\014\255\
\120\255\000\000\144\255\170\000\143\255\086\255\001\255\019\255\
\101\255\101\255\101\255\101\255\101\255\101\255\086\255\010\255\
\010\255\010\255\001\255\101\255\047\255\000\000\067\255\145\255\
\170\255\000\000\000\000\086\255\000\000\121\255\000\000\000\000\
\000\000\124\255\128\255\124\255\124\255\124\255\124\255\000\000\
\116\255\000\000\000\000\000\000\000\000\119\255\151\255\152\255\
\000\000\121\255\019\255\019\255\019\255\149\255\153\255\154\255\
\000\000\000\000\157\255\158\255\019\255\156\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\062\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\162\255\
\168\255\169\255\000\000\054\255\000\000\000\000\000\000\000\000\
\160\255\000\000\000\000\000\000\000\000\000\000\000\000\006\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\132\255\000\000\000\000\
\000\000\091\255\093\255\108\255\113\255\114\255\115\255\000\000\
\150\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\171\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\172\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\226\255\253\255\000\000\000\000\000\000\000\000\247\255\
\000\000\000\000\069\000\235\255\118\000\156\000\132\000"

let yytablesize = 189
let yytable = "\016\000\
\029\000\005\000\006\000\007\000\008\000\009\000\028\000\030\000\
\031\000\064\000\033\000\006\000\006\000\001\000\005\000\006\000\
\062\000\008\000\009\000\005\000\006\000\007\000\008\000\009\000\
\014\000\072\000\010\000\011\000\054\000\063\000\006\000\003\000\
\034\000\014\000\061\000\028\000\016\000\012\000\082\000\025\000\
\013\000\077\000\014\000\061\000\073\000\073\000\073\000\028\000\
\006\000\079\000\052\000\053\000\086\000\087\000\088\000\035\000\
\061\000\025\000\025\000\025\000\025\000\025\000\094\000\032\000\
\025\000\039\000\039\000\039\000\025\000\048\000\049\000\050\000\
\025\000\039\000\039\000\039\000\025\000\041\000\032\000\016\000\
\016\000\016\000\025\000\025\000\025\000\025\000\025\000\006\000\
\007\000\016\000\025\000\025\000\025\000\025\000\025\000\042\000\
\043\000\044\000\045\000\046\000\055\000\005\000\006\000\026\000\
\036\000\036\000\040\000\026\000\037\000\066\000\067\000\068\000\
\069\000\070\000\071\000\038\000\026\000\036\000\027\000\026\000\
\078\000\026\000\027\000\028\000\029\000\030\000\039\000\028\000\
\029\000\030\000\047\000\027\000\038\000\051\000\027\000\057\000\
\028\000\029\000\030\000\028\000\029\000\030\000\083\000\045\000\
\045\000\045\000\045\000\042\000\043\000\044\000\045\000\046\000\
\042\000\043\000\044\000\045\000\046\000\043\000\044\000\045\000\
\046\000\039\000\039\000\039\000\039\000\074\000\075\000\076\000\
\058\000\059\000\060\000\080\000\033\000\089\000\084\000\085\000\
\013\000\090\000\091\000\092\000\095\000\093\000\014\000\015\000\
\037\000\018\000\022\000\056\000\081\000"

let yycheck = "\003\000\
\010\000\001\001\002\001\003\001\004\001\005\001\010\000\011\000\
\012\000\040\000\003\001\002\001\007\001\001\000\001\001\002\001\
\038\000\004\001\005\001\001\001\002\001\003\001\004\001\005\001\
\024\001\047\000\008\001\009\001\032\000\039\000\025\001\006\001\
\025\001\024\001\038\000\039\000\040\000\019\001\060\000\011\001\
\022\001\051\000\024\001\047\000\048\000\049\000\050\000\051\000\
\002\001\053\000\020\001\021\001\083\000\084\000\085\000\027\001\
\060\000\029\001\030\001\031\001\032\001\033\001\093\000\010\001\
\011\001\012\001\013\001\014\001\015\001\012\001\013\001\014\001\
\011\001\012\001\013\001\014\001\015\001\011\001\026\001\083\000\
\084\000\085\000\029\001\030\001\031\001\032\001\033\001\002\001\
\003\001\093\000\029\001\030\001\031\001\032\001\033\001\029\001\
\030\001\031\001\032\001\033\001\032\000\001\001\002\001\011\001\
\007\001\015\001\015\001\015\001\010\001\041\000\042\000\043\000\
\044\000\045\000\046\000\017\001\024\001\027\001\011\001\027\001\
\052\000\029\001\015\001\011\001\011\001\011\001\028\001\015\001\
\015\001\015\001\010\001\024\001\017\001\028\001\027\001\016\001\
\024\001\024\001\024\001\027\001\027\001\027\001\024\001\012\001\
\013\001\014\001\015\001\029\001\030\001\031\001\032\001\033\001\
\029\001\030\001\031\001\032\001\033\001\030\001\031\001\032\001\
\033\001\012\001\013\001\014\001\015\001\048\000\049\000\050\000\
\025\001\000\000\028\001\027\001\003\001\025\001\024\001\024\001\
\015\001\025\001\025\001\023\001\025\001\024\001\015\001\015\001\
\025\001\015\001\015\001\032\000\057\000"

let yynames_const = "\
  BEGIN\000\
  END\000\
  PRINT\000\
  VAR_DEC\000\
  CONCAT\000\
  LT\000\
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
  IF\000\
  ELSE\000\
  LCURLY\000\
  RCURLY\000\
  LPAREN\000\
  RPAREN\000\
  EQUALS\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  LITERAL\000\
  TRUE\000\
  FALSE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'statements) in
    Obj.repr(
# 34 "parser.mly"
                             (_2)
# 249 "parser.ml"
               : ParseTree.tTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
          (TermVar(_1))
# 256 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
            (TermString(_1))
# 263 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 46 "parser.mly"
         (TermBool(_1))
# 270 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 47 "parser.mly"
          (TermBool(_1))
# 277 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 51 "parser.mly"
                      (_1)
# 284 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 52 "parser.mly"
                                 ( MultiStatement (_1, _3) )
# 292 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec_op) in
    Obj.repr(
# 56 "parser.mly"
           ( _1 )
# 299 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 57 "parser.mly"
              ( _1 )
# 306 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mut_op) in
    Obj.repr(
# 58 "parser.mly"
           ( _1 )
# 313 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exec_op) in
    Obj.repr(
# 59 "parser.mly"
            ( _1 )
# 320 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 63 "parser.mly"
                                    ( Declaration (_2, _4) )
# 328 "parser.ml"
               : 'dec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 67 "parser.mly"
                  (_1)
# 335 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 68 "parser.mly"
                  (_1)
# 342 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 69 "parser.mly"
                  (_1)
# 349 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_opreration) in
    Obj.repr(
# 70 "parser.mly"
                    (_1)
# 356 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 74 "parser.mly"
                           ( TermMut (_1, _3) )
# 364 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 75 "parser.mly"
                                      ( TermMut(_1, TermConcat (_1, _4)) )
# 372 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 79 "parser.mly"
                    ( PrintOperation (_2))
# 379 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 80 "parser.mly"
                                                ( ForOperation (_2, _4, _6))
# 388 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'int_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 81 "parser.mly"
                                                        ( ForLoop ( _2, _4, _6))
# 397 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bool_opreration) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 82 "parser.mly"
                                                               ( IfStatement (_3, _6) )
# 405 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'bool_opreration) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'statements) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 83 "parser.mly"
                                                                                            ( IfElseStatement (_3, _6, _10) )
# 414 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 87 "parser.mly"
        (TermInteger(_1))
# 421 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 88 "parser.mly"
          (_1)
# 428 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 89 "parser.mly"
                                     (TermPlus(_1,_3))
# 436 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 90 "parser.mly"
                                      (TermMinus(_1,_3))
# 444 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 91 "parser.mly"
                                      (TermMult(_1,_3))
# 452 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 92 "parser.mly"
                                    (TermDiv(_1,_3))
# 460 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 93 "parser.mly"
                                    (TermMod(_1,_3))
# 468 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 97 "parser.mly"
            (_1)
# 475 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 98 "parser.mly"
            (_1)
# 482 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 99 "parser.mly"
                                       (TermConcat (_1, _3) )
# 490 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean) in
    Obj.repr(
# 103 "parser.mly"
            (_1)
# 497 "parser.ml"
               : 'bool_opreration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 104 "parser.mly"
          (_1)
# 504 "parser.ml"
               : 'bool_opreration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 105 "parser.mly"
                                   (TermLt(_1,_3))
# 512 "parser.ml"
               : 'bool_opreration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                (_1 :: [] )
# 519 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 110 "parser.mly"
                           ( _1 :: _3 )
# 527 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 114 "parser.mly"
          ( _1 )
# 534 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                  ( TermArgs([]) )
# 540 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 116 "parser.mly"
                       (TermArgs( _2 ) )
# 547 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 117 "parser.mly"
                                      ( TermUnion (_1, _3) )
# 555 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 118 "parser.mly"
                                          ( TermIntersection ( _1, _3 ) )
# 563 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 119 "parser.mly"
                                     ( TermDifference ( _1, _3 ) )
# 571 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 120 "parser.mly"
                            ( TermAdd (_1, _3) )
# 579 "parser.ml"
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
