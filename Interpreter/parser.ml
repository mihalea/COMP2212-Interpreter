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
\014\000\014\000\014\000\014\000\014\000\014\000\015\000\015\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\001\000\001\000\002\000\003\000\001\000\
\001\000\001\000\001\000\004\000\001\000\001\000\001\000\001\000\
\003\000\004\000\002\000\007\000\007\000\007\000\011\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\001\000\001\000\
\003\000\001\000\001\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\003\000\
\001\000\002\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\056\000\024\000\002\000\003\000\004\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\034\000\000\000\008\000\009\000\010\000\011\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\036\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\001\000\000\000\
\032\000\000\000\017\000\007\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\000\000\000\000\052\000\053\000\054\000\042\000\
\045\000\012\000\000\000\000\000\000\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\020\000\000\000\
\000\000\000\000\000\000\023\000"

let yydgoto = "\002\000\
\004\000\016\000\077\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\040\000"

let yysindex = "\001\000\
\017\255\000\000\175\255\000\000\000\000\000\000\000\000\000\000\
\000\000\013\255\010\255\184\255\010\255\252\254\000\255\026\255\
\001\255\000\000\000\000\015\255\000\000\000\000\000\000\000\000\
\071\255\034\255\148\255\058\255\023\255\000\000\014\255\000\000\
\071\255\034\255\000\000\167\255\184\255\016\255\000\000\020\255\
\063\000\044\255\226\255\013\255\175\255\236\255\236\255\236\255\
\236\255\236\255\236\255\236\255\236\255\236\255\236\255\236\255\
\226\255\226\255\226\255\002\255\002\255\002\255\184\255\184\255\
\013\255\236\255\010\255\065\255\085\255\000\000\000\000\226\255\
\000\000\083\255\000\000\000\000\000\000\089\255\089\255\089\255\
\089\255\089\255\089\255\117\255\089\255\089\255\089\255\089\255\
\000\000\083\255\083\255\023\255\000\000\000\000\000\000\000\000\
\000\000\000\000\160\255\069\255\099\255\000\000\083\255\175\255\
\175\255\175\255\101\255\110\255\127\255\000\000\000\000\152\255\
\161\255\175\255\171\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\195\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\183\255\191\255\214\255\219\255\124\255\000\000\000\000\136\255\
\000\000\000\000\000\000\000\000\000\000\228\255\000\000\000\000\
\000\000\000\000\000\000\000\000\254\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\205\255\000\000\000\000\000\000\004\255\006\255\056\255\
\079\255\149\255\012\000\206\255\231\255\242\255\255\255\010\000\
\000\000\020\000\023\000\230\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\239\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\241\255\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\217\255\253\255\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\067\000\033\000\242\000\245\255\196\000"

let yytablesize = 312
let yytable = "\017\000\
\035\000\001\000\038\000\006\000\006\000\076\000\029\000\031\000\
\032\000\036\000\042\000\006\000\030\000\005\000\006\000\007\000\
\008\000\009\000\037\000\037\000\038\000\038\000\003\000\043\000\
\037\000\068\000\038\000\037\000\006\000\012\000\039\000\015\000\
\041\000\032\000\044\000\045\000\037\000\069\000\038\000\073\000\
\029\000\017\000\015\000\057\000\034\000\043\000\075\000\065\000\
\058\000\059\000\070\000\096\000\097\000\073\000\073\000\073\000\
\092\000\092\000\092\000\032\000\032\000\029\000\071\000\100\000\
\107\000\108\000\109\000\098\000\073\000\034\000\039\000\039\000\
\063\000\064\000\115\000\074\000\039\000\072\000\033\000\063\000\
\064\000\046\000\047\000\048\000\049\000\050\000\051\000\038\000\
\039\000\089\000\090\000\091\000\057\000\040\000\040\000\034\000\
\034\000\101\000\105\000\040\000\017\000\017\000\017\000\033\000\
\103\000\052\000\053\000\054\000\055\000\056\000\017\000\040\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\052\000\053\000\054\000\055\000\056\000\
\106\000\033\000\033\000\110\000\099\000\032\000\025\000\025\000\
\025\000\025\000\025\000\025\000\111\000\049\000\049\000\049\000\
\025\000\032\000\025\000\025\000\025\000\025\000\025\000\025\000\
\053\000\054\000\055\000\056\000\035\000\112\000\025\000\025\000\
\025\000\025\000\025\000\041\000\041\000\060\000\061\000\062\000\
\035\000\041\000\025\000\025\000\025\000\025\000\025\000\005\000\
\006\000\007\000\008\000\009\000\113\000\041\000\010\000\011\000\
\005\000\006\000\007\000\008\000\009\000\104\000\114\000\012\000\
\066\000\067\000\052\000\053\000\054\000\055\000\056\000\013\000\
\012\000\116\000\014\000\013\000\015\000\025\000\025\000\025\000\
\025\000\025\000\025\000\014\000\049\000\049\000\049\000\025\000\
\026\000\026\000\026\000\026\000\026\000\026\000\055\000\055\000\
\055\000\055\000\026\000\006\000\007\000\025\000\025\000\025\000\
\025\000\025\000\015\000\026\000\005\000\006\000\026\000\016\000\
\026\000\027\000\027\000\027\000\027\000\027\000\027\000\049\000\
\049\000\049\000\049\000\027\000\028\000\028\000\028\000\028\000\
\028\000\028\000\047\000\018\000\027\000\022\000\028\000\027\000\
\102\000\029\000\029\000\029\000\029\000\029\000\029\000\028\000\
\000\000\000\000\028\000\029\000\030\000\030\000\030\000\030\000\
\030\000\030\000\044\000\044\000\029\000\000\000\030\000\029\000\
\044\000\000\000\043\000\043\000\000\000\046\000\046\000\030\000\
\043\000\000\000\030\000\046\000\044\000\093\000\094\000\095\000\
\000\000\000\000\000\000\000\000\043\000\000\000\000\000\046\000"

let yycheck = "\003\000\
\012\000\001\000\003\001\002\001\007\001\045\000\010\000\011\000\
\012\000\013\000\010\001\002\001\010\000\001\001\002\001\003\001\
\004\001\005\001\015\001\016\001\015\001\016\001\006\001\023\001\
\021\001\037\000\021\001\032\001\031\001\017\001\031\001\030\001\
\007\001\037\000\034\001\021\001\033\001\022\001\033\001\043\000\
\044\000\045\000\030\001\010\001\012\000\023\001\044\000\034\001\
\015\001\016\001\031\001\063\000\064\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\000\000\067\000\
\104\000\105\000\106\000\065\000\072\000\037\000\015\001\016\001\
\015\001\016\001\114\000\043\000\021\001\034\001\012\000\015\001\
\016\001\011\001\012\001\013\001\014\001\015\001\016\001\003\001\
\033\001\057\000\058\000\059\000\010\001\015\001\016\001\063\000\
\064\000\033\001\030\001\021\001\104\000\105\000\106\000\037\000\
\072\000\035\001\036\001\037\001\038\001\039\001\114\000\033\001\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\035\001\036\001\037\001\038\001\039\001\
\030\001\063\000\064\000\031\001\066\000\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\031\001\018\001\019\001\020\001\
\021\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\036\001\037\001\038\001\039\001\021\001\031\001\035\001\036\001\
\037\001\038\001\039\001\015\001\016\001\018\001\019\001\020\001\
\033\001\021\001\035\001\036\001\037\001\038\001\039\001\001\001\
\002\001\003\001\004\001\005\001\029\001\033\001\008\001\009\001\
\001\001\002\001\003\001\004\001\005\001\030\001\030\001\017\001\
\026\001\027\001\035\001\036\001\037\001\038\001\039\001\025\001\
\017\001\031\001\028\001\021\001\030\001\011\001\012\001\013\001\
\014\001\015\001\016\001\021\001\018\001\019\001\020\001\021\001\
\011\001\012\001\013\001\014\001\015\001\016\001\018\001\019\001\
\020\001\021\001\021\001\002\001\003\001\035\001\036\001\037\001\
\038\001\039\001\021\001\030\001\001\001\002\001\033\001\021\001\
\035\001\011\001\012\001\013\001\014\001\015\001\016\001\018\001\
\019\001\020\001\021\001\021\001\011\001\012\001\013\001\014\001\
\015\001\016\001\031\001\021\001\030\001\021\001\021\001\033\001\
\069\000\011\001\012\001\013\001\014\001\015\001\016\001\030\001\
\255\255\255\255\033\001\021\001\011\001\012\001\013\001\014\001\
\015\001\016\001\015\001\016\001\030\001\255\255\021\001\033\001\
\021\001\255\255\015\001\016\001\255\255\015\001\016\001\030\001\
\021\001\255\255\033\001\021\001\033\001\060\000\061\000\062\000\
\255\255\255\255\255\255\255\255\033\001\255\255\255\255\033\001"

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
# 308 "parser.ml"
               : ParseTree.tTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "parser.mly"
          (TermVar(_1))
# 315 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
            (TermString(_1))
# 322 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 48 "parser.mly"
         (TermBool(_1))
# 329 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 49 "parser.mly"
          (TermBool(_1))
# 336 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 53 "parser.mly"
                      (_1)
# 343 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 54 "parser.mly"
                                 ( MultiStatement (_1, _3) )
# 351 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec_op) in
    Obj.repr(
# 58 "parser.mly"
           ( _1 )
# 358 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 59 "parser.mly"
              ( _1 )
# 365 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mut_op) in
    Obj.repr(
# 60 "parser.mly"
           ( _1 )
# 372 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exec_op) in
    Obj.repr(
# 61 "parser.mly"
            ( _1 )
# 379 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 65 "parser.mly"
                                    ( Declaration (_2, _4) )
# 387 "parser.ml"
               : 'dec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 69 "parser.mly"
                  (_1)
# 394 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 70 "parser.mly"
                  (_1)
# 401 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 71 "parser.mly"
                  (_1)
# 408 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 72 "parser.mly"
                   (_1)
# 415 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 76 "parser.mly"
                           ( TermMut (_1, _3) )
# 423 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 77 "parser.mly"
                                      ( TermMut(_1, TermConcat (_1, _4)) )
# 431 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 81 "parser.mly"
                    ( PrintOperation (_2))
# 438 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 82 "parser.mly"
                                                ( ForOperation (_2, _4, _6))
# 447 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'int_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 83 "parser.mly"
                                                        ( ForLoop ( _2, _4, _6))
# 456 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bool_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 84 "parser.mly"
                                                              ( IfStatement (_3, _6) )
# 464 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'bool_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'statements) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 85 "parser.mly"
                                                                                           ( IfElseStatement (_3, _6, _10) )
# 473 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 89 "parser.mly"
        (TermInteger(_1))
# 480 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 90 "parser.mly"
          (_1)
# 487 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 91 "parser.mly"
                                     (TermPlus(_1,_3))
# 495 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 92 "parser.mly"
                                      (TermMinus(_1,_3))
# 503 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 93 "parser.mly"
                                      (TermMult(_1,_3))
# 511 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 94 "parser.mly"
                                    (TermDiv(_1,_3))
# 519 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 95 "parser.mly"
                                    (TermMod(_1,_3))
# 527 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 99 "parser.mly"
            (_1)
# 534 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 100 "parser.mly"
            (_1)
# 541 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 101 "parser.mly"
                                       (TermConcat (_1, _3) )
# 549 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean) in
    Obj.repr(
# 105 "parser.mly"
            (_1)
# 556 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 106 "parser.mly"
          (_1)
# 563 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 107 "parser.mly"
                        (TermNot(_2))
# 570 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 108 "parser.mly"
                                   (TermLt(_1,_3))
# 578 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 109 "parser.mly"
                                    (TermLte(_1,_3))
# 586 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 110 "parser.mly"
                                   (TermGt(_1,_3))
# 594 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 111 "parser.mly"
                                    (TermGte(_1,_3))
# 602 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 112 "parser.mly"
                                   (TermEq(_1,_3))
# 610 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 113 "parser.mly"
                                     (TermEq(_1,_3))
# 618 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 114 "parser.mly"
                                   (TermEq(_1,_3))
# 626 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 115 "parser.mly"
                                    (TermNeq(_1,_3))
# 634 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 116 "parser.mly"
                                      (TermNeq(_1,_3))
# 642 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 117 "parser.mly"
                                    (TermNeq(_1,_3))
# 650 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                (_1 :: [] )
# 657 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 122 "parser.mly"
                           ( _1 :: _3 )
# 665 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 126 "parser.mly"
          ( _1 )
# 672 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
                  ( TermArgs([]) )
# 678 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 128 "parser.mly"
                       (TermArgs( _2 ) )
# 685 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 129 "parser.mly"
                                      ( TermUnion (_1, _3) )
# 693 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 130 "parser.mly"
                                          ( TermIntersection ( _1, _3 ) )
# 701 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 131 "parser.mly"
                                     ( TermDifference ( _1, _3 ) )
# 709 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 132 "parser.mly"
                            ( TermAdd (_1, _3) )
# 717 "parser.ml"
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
