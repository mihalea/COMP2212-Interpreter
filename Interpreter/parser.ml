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
  | LTE
  | GT
  | GTE
  | EQ
  | NEQ
  | NOT
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
# 48 "parser.ml"
let yytransl_const = [|
  262 (* BEGIN *);
  263 (* END *);
  264 (* PRINT *);
  265 (* VAR_DEC *);
  266 (* CONCAT *);
  267 (* LT *);
  268 (* LTE *);
  269 (* GT *);
  270 (* GTE *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* NOT *);
  274 (* UNION *);
  275 (* INTERSECT *);
  276 (* DIFF *);
  277 (* SEMICOL *);
  278 (* COMMA *);
  279 (* ADD *);
    0 (* EOF *);
  280 (* EOL *);
  281 (* FOR *);
  282 (* TO *);
  283 (* IN *);
  284 (* IF *);
  285 (* ELSE *);
  286 (* LCURLY *);
  287 (* RCURLY *);
  288 (* LPAREN *);
  289 (* RPAREN *);
  290 (* EQUALS *);
  291 (* PLUS *);
  292 (* MINUS *);
  293 (* TIMES *);
  294 (* DIV *);
  295 (* MOD *);
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
\012\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\015\000\015\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\001\000\001\000\002\000\003\000\001\000\
\001\000\001\000\001\000\004\000\001\000\001\000\001\000\001\000\
\003\000\004\000\002\000\007\000\007\000\007\000\011\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\001\000\001\000\
\003\000\001\000\001\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\003\000\001\000\002\000\
\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\054\000\024\000\002\000\003\000\004\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\034\000\000\000\008\000\009\000\010\000\011\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\000\001\000\000\000\032\000\000\000\017\000\
\007\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\000\000\050\000\
\051\000\052\000\042\000\044\000\012\000\000\000\000\000\000\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\020\000\000\000\000\000\000\000\000\000\023\000"

let yydgoto = "\002\000\
\004\000\016\000\074\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\039\000"

let yysindex = "\023\000\
\026\255\000\000\018\255\000\000\000\000\000\000\000\000\000\000\
\000\000\001\255\032\255\078\255\032\255\004\255\011\255\055\255\
\037\255\000\000\000\000\043\255\000\000\000\000\000\000\000\000\
\126\255\060\255\010\255\000\255\053\255\000\000\044\255\000\000\
\126\255\000\000\077\255\078\255\059\255\000\000\070\255\105\000\
\080\255\128\255\001\255\018\255\158\255\158\255\158\255\158\255\
\158\255\158\255\158\255\158\255\158\255\158\255\158\255\128\255\
\015\255\015\255\015\255\078\255\078\255\001\255\158\255\032\255\
\151\255\122\255\000\000\000\000\128\255\000\000\060\255\000\000\
\000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\118\255\255\255\255\255\255\255\255\255\000\000\053\255\000\000\
\000\000\000\000\000\000\000\000\000\000\243\255\138\255\139\255\
\000\000\060\255\018\255\018\255\018\255\140\255\152\255\154\255\
\000\000\000\000\157\255\164\255\018\255\169\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\108\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\174\255\176\255\183\255\184\255\097\255\000\000\000\000\137\255\
\000\000\000\000\000\000\000\000\175\255\000\000\000\000\000\000\
\000\000\000\000\000\000\006\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\187\255\219\255\221\255\241\255\244\255\254\255\
\166\255\177\255\200\255\211\255\234\255\000\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\188\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\196\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\224\255\253\255\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\039\000\252\255\015\000\245\255\152\000"

let yytablesize = 298
let yytable = "\017\000\
\034\000\005\000\006\000\007\000\008\000\009\000\029\000\031\000\
\032\000\035\000\030\000\073\000\006\000\037\000\060\000\061\000\
\006\000\012\000\005\000\006\000\007\000\008\000\009\000\001\000\
\065\000\010\000\011\000\057\000\058\000\059\000\015\000\003\000\
\032\000\006\000\012\000\036\000\006\000\071\000\070\000\029\000\
\017\000\038\000\013\000\072\000\015\000\014\000\041\000\015\000\
\091\000\092\000\033\000\086\000\070\000\087\000\087\000\087\000\
\032\000\032\000\029\000\042\000\095\000\040\000\093\000\044\000\
\098\000\070\000\102\000\103\000\104\000\056\000\043\000\088\000\
\089\000\090\000\033\000\042\000\110\000\062\000\005\000\006\000\
\066\000\008\000\009\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\012\000\017\000\
\017\000\017\000\033\000\033\000\067\000\094\000\063\000\064\000\
\068\000\017\000\032\000\025\000\025\000\025\000\025\000\025\000\
\025\000\069\000\047\000\047\000\047\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\037\000\047\000\047\000\047\000\
\025\000\006\000\007\000\025\000\025\000\025\000\025\000\025\000\
\045\000\046\000\047\000\048\000\049\000\050\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
\025\000\052\000\053\000\054\000\055\000\035\000\005\000\006\000\
\051\000\052\000\053\000\054\000\055\000\060\000\061\000\100\000\
\101\000\035\000\105\000\025\000\025\000\025\000\025\000\025\000\
\026\000\026\000\026\000\026\000\026\000\026\000\106\000\096\000\
\107\000\108\000\026\000\027\000\027\000\027\000\027\000\027\000\
\027\000\109\000\013\000\026\000\014\000\027\000\026\000\111\000\
\026\000\037\000\037\000\015\000\016\000\045\000\027\000\037\000\
\018\000\027\000\028\000\028\000\028\000\028\000\028\000\028\000\
\022\000\097\000\000\000\037\000\028\000\029\000\029\000\029\000\
\029\000\029\000\029\000\000\000\000\000\028\000\000\000\029\000\
\028\000\038\000\038\000\039\000\039\000\000\000\000\000\038\000\
\029\000\039\000\000\000\029\000\030\000\030\000\030\000\030\000\
\030\000\030\000\000\000\038\000\000\000\039\000\030\000\040\000\
\040\000\000\000\041\000\041\000\000\000\040\000\000\000\030\000\
\041\000\000\000\030\000\000\000\043\000\043\000\000\000\000\000\
\099\000\040\000\043\000\000\000\041\000\051\000\052\000\053\000\
\054\000\055\000\053\000\053\000\053\000\053\000\043\000\000\000\
\000\000\051\000\052\000\053\000\054\000\055\000\047\000\047\000\
\047\000\047\000"

let yycheck = "\003\000\
\012\000\001\001\002\001\003\001\004\001\005\001\010\000\011\000\
\012\000\013\000\010\000\044\000\007\001\003\001\015\001\016\001\
\002\001\017\001\001\001\002\001\003\001\004\001\005\001\001\000\
\036\000\008\001\009\001\018\001\019\001\020\001\030\001\006\001\
\036\000\002\001\017\001\032\001\031\001\042\000\042\000\043\000\
\044\000\031\001\025\001\043\000\030\001\028\001\010\001\030\001\
\060\000\061\000\012\000\056\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\023\001\064\000\007\001\062\000\021\001\
\069\000\069\000\099\000\100\000\101\000\010\001\034\001\057\000\
\058\000\059\000\036\000\023\001\109\000\034\001\001\001\002\001\
\022\001\004\001\005\001\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\017\001\099\000\
\100\000\101\000\060\000\061\000\031\001\063\000\026\001\027\001\
\000\000\109\000\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\034\001\018\001\019\001\020\001\021\001\011\001\012\001\
\013\001\014\001\015\001\016\001\003\001\018\001\019\001\020\001\
\021\001\002\001\003\001\035\001\036\001\037\001\038\001\039\001\
\011\001\012\001\013\001\014\001\015\001\016\001\035\001\036\001\
\037\001\038\001\039\001\011\001\012\001\013\001\014\001\015\001\
\016\001\036\001\037\001\038\001\039\001\021\001\001\001\002\001\
\035\001\036\001\037\001\038\001\039\001\015\001\016\001\030\001\
\030\001\033\001\031\001\035\001\036\001\037\001\038\001\039\001\
\011\001\012\001\013\001\014\001\015\001\016\001\031\001\033\001\
\031\001\029\001\021\001\011\001\012\001\013\001\014\001\015\001\
\016\001\030\001\021\001\030\001\021\001\021\001\033\001\031\001\
\035\001\015\001\016\001\021\001\021\001\031\001\030\001\021\001\
\021\001\033\001\011\001\012\001\013\001\014\001\015\001\016\001\
\021\001\066\000\255\255\033\001\021\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\030\001\255\255\021\001\
\033\001\015\001\016\001\015\001\016\001\255\255\255\255\021\001\
\030\001\021\001\255\255\033\001\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\033\001\255\255\033\001\021\001\015\001\
\016\001\255\255\015\001\016\001\255\255\021\001\255\255\030\001\
\021\001\255\255\033\001\255\255\015\001\016\001\255\255\255\255\
\030\001\033\001\021\001\255\255\033\001\035\001\036\001\037\001\
\038\001\039\001\018\001\019\001\020\001\021\001\033\001\255\255\
\255\255\035\001\036\001\037\001\038\001\039\001\018\001\019\001\
\020\001\021\001"

let yynames_const = "\
  BEGIN\000\
  END\000\
  PRINT\000\
  VAR_DEC\000\
  CONCAT\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  EQ\000\
  NEQ\000\
  NOT\000\
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
# 36 "parser.mly"
                             (_2)
# 303 "parser.ml"
               : ParseTree.tTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "parser.mly"
          (TermVar(_1))
# 310 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
            (TermString(_1))
# 317 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 48 "parser.mly"
         (TermBool(_1))
# 324 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 49 "parser.mly"
          (TermBool(_1))
# 331 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 53 "parser.mly"
                      (_1)
# 338 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 54 "parser.mly"
                                 ( MultiStatement (_1, _3) )
# 346 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec_op) in
    Obj.repr(
# 58 "parser.mly"
           ( _1 )
# 353 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 59 "parser.mly"
              ( _1 )
# 360 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mut_op) in
    Obj.repr(
# 60 "parser.mly"
           ( _1 )
# 367 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exec_op) in
    Obj.repr(
# 61 "parser.mly"
            ( _1 )
# 374 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 65 "parser.mly"
                                    ( Declaration (_2, _4) )
# 382 "parser.ml"
               : 'dec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 69 "parser.mly"
                  (_1)
# 389 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 70 "parser.mly"
                  (_1)
# 396 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 71 "parser.mly"
                  (_1)
# 403 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 72 "parser.mly"
                   (_1)
# 410 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 76 "parser.mly"
                           ( TermMut (_1, _3) )
# 418 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 77 "parser.mly"
                                      ( TermMut(_1, TermConcat (_1, _4)) )
# 426 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 81 "parser.mly"
                    ( PrintOperation (_2))
# 433 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 82 "parser.mly"
                                                ( ForOperation (_2, _4, _6))
# 442 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'int_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 83 "parser.mly"
                                                        ( ForLoop ( _2, _4, _6))
# 451 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bool_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 84 "parser.mly"
                                                              ( IfStatement (_3, _6) )
# 459 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'bool_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'statements) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 85 "parser.mly"
                                                                                           ( IfElseStatement (_3, _6, _10) )
# 468 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 89 "parser.mly"
        (TermInteger(_1))
# 475 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 90 "parser.mly"
          (_1)
# 482 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 91 "parser.mly"
                                     (TermPlus(_1,_3))
# 490 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 92 "parser.mly"
                                      (TermMinus(_1,_3))
# 498 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 93 "parser.mly"
                                      (TermMult(_1,_3))
# 506 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 94 "parser.mly"
                                    (TermDiv(_1,_3))
# 514 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 95 "parser.mly"
                                    (TermMod(_1,_3))
# 522 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 99 "parser.mly"
            (_1)
# 529 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 100 "parser.mly"
            (_1)
# 536 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 101 "parser.mly"
                                       (TermConcat (_1, _3) )
# 544 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean) in
    Obj.repr(
# 105 "parser.mly"
            (_1)
# 551 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 106 "parser.mly"
          (_1)
# 558 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 107 "parser.mly"
                        (TermNot(_2))
# 565 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 108 "parser.mly"
                                   (TermLt(_1,_3))
# 573 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 109 "parser.mly"
                                    (TermLte(_1,_3))
# 581 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 110 "parser.mly"
                                   (TermGt(_1,_3))
# 589 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 111 "parser.mly"
                                    (TermGte(_1,_3))
# 597 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 112 "parser.mly"
                                   (TermEq(_1,_3))
# 605 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 113 "parser.mly"
                                     (TermEq(_1,_3))
# 613 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 114 "parser.mly"
                                    (TermNeq(_1,_3))
# 621 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 115 "parser.mly"
                                      (TermNeq(_1,_3))
# 629 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
                (_1 :: [] )
# 636 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 120 "parser.mly"
                           ( _1 :: _3 )
# 644 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 124 "parser.mly"
          ( _1 )
# 651 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                  ( TermArgs([]) )
# 657 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 126 "parser.mly"
                       (TermArgs( _2 ) )
# 664 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 127 "parser.mly"
                                      ( TermUnion (_1, _3) )
# 672 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 128 "parser.mly"
                                          ( TermIntersection ( _1, _3 ) )
# 680 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 129 "parser.mly"
                                     ( TermDifference ( _1, _3 ) )
# 688 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 130 "parser.mly"
                            ( TermAdd (_1, _3) )
# 696 "parser.ml"
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
