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
\009\000\009\000\010\000\010\000\010\000\010\000\010\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\013\000\013\000\
\013\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\015\000\015\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\000\000"

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
\036\000\000\000\000\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\001\000\000\000\
\032\000\000\000\017\000\007\000\042\000\045\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\052\000\053\000\
\054\000\012\000\000\000\000\000\000\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\020\000\000\000\
\000\000\000\000\000\000\023\000"

let yydgoto = "\002\000\
\004\000\016\000\079\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\040\000"

let yysindex = "\031\000\
\033\255\000\000\082\255\000\000\000\000\000\000\000\000\000\000\
\000\000\016\255\003\255\186\255\003\255\005\255\255\254\048\255\
\004\255\000\000\000\000\042\255\000\000\000\000\000\000\000\000\
\000\255\170\255\038\255\129\255\055\255\000\000\054\255\000\000\
\000\000\170\255\038\255\047\255\186\255\060\255\000\000\069\255\
\104\000\116\255\106\255\016\255\082\255\186\255\186\255\112\255\
\112\255\112\255\112\255\112\255\112\255\112\255\112\255\112\255\
\112\255\112\255\106\255\106\255\106\255\001\255\001\255\001\255\
\016\255\112\255\003\255\252\254\149\255\000\000\000\000\106\255\
\000\000\182\255\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\023\000\023\000\023\000\242\254\023\000\023\000\
\023\000\023\000\000\000\182\255\182\255\055\255\000\000\000\000\
\000\000\000\000\013\000\163\255\169\255\000\000\182\255\082\255\
\082\255\082\255\171\255\173\255\185\255\000\000\000\000\172\255\
\187\255\082\255\188\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\159\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\179\255\197\255\206\255\207\255\118\255\000\000\000\000\130\255\
\000\000\000\000\000\000\000\000\000\000\202\255\000\000\000\000\
\000\000\000\000\000\000\000\000\253\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\255\000\000\000\000\000\000\000\000\000\000\056\255\
\143\255\220\255\246\255\004\000\011\000\199\255\210\255\233\255\
\244\255\001\000\000\000\014\000\024\000\077\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\209\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\216\255\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\217\255\253\255\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\245\255\069\000\033\000\098\000\169\000"

let yytablesize = 318
let yytable = "\017\000\
\033\000\038\000\006\000\006\000\006\000\076\000\029\000\031\000\
\032\000\036\000\046\000\047\000\030\000\042\000\046\000\047\000\
\005\000\006\000\007\000\008\000\009\000\055\000\056\000\057\000\
\058\000\068\000\043\000\006\000\101\000\039\000\015\000\001\000\
\012\000\032\000\077\000\078\000\037\000\044\000\003\000\073\000\
\029\000\017\000\032\000\032\000\035\000\015\000\075\000\059\000\
\055\000\055\000\055\000\055\000\060\000\061\000\041\000\073\000\
\073\000\073\000\094\000\094\000\094\000\029\000\045\000\100\000\
\107\000\108\000\109\000\098\000\073\000\035\000\037\000\037\000\
\066\000\067\000\115\000\074\000\037\000\043\000\035\000\035\000\
\034\000\069\000\005\000\006\000\007\000\008\000\009\000\065\000\
\037\000\010\000\011\000\091\000\092\000\093\000\049\000\049\000\
\049\000\049\000\012\000\070\000\017\000\017\000\017\000\071\000\
\103\000\034\000\013\000\006\000\007\000\014\000\017\000\015\000\
\005\000\006\000\034\000\034\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\032\000\
\025\000\025\000\025\000\025\000\025\000\025\000\099\000\049\000\
\049\000\049\000\025\000\032\000\025\000\025\000\025\000\025\000\
\025\000\025\000\062\000\063\000\064\000\072\000\035\000\038\000\
\025\000\025\000\025\000\025\000\025\000\038\000\038\000\095\000\
\096\000\097\000\035\000\038\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\038\000\
\049\000\049\000\049\000\025\000\048\000\049\000\050\000\051\000\
\052\000\053\000\005\000\006\000\007\000\008\000\009\000\059\000\
\105\000\025\000\025\000\025\000\025\000\025\000\106\000\013\000\
\113\000\110\000\012\000\111\000\054\000\055\000\056\000\057\000\
\058\000\026\000\026\000\026\000\026\000\026\000\026\000\112\000\
\114\000\014\000\116\000\026\000\027\000\027\000\027\000\027\000\
\027\000\027\000\015\000\016\000\026\000\018\000\027\000\026\000\
\047\000\026\000\039\000\039\000\022\000\102\000\000\000\027\000\
\039\000\000\000\027\000\028\000\028\000\028\000\028\000\028\000\
\028\000\000\000\000\000\000\000\039\000\028\000\029\000\029\000\
\029\000\029\000\029\000\029\000\040\000\040\000\028\000\000\000\
\029\000\028\000\040\000\030\000\030\000\030\000\030\000\030\000\
\030\000\029\000\041\000\041\000\029\000\030\000\040\000\000\000\
\041\000\044\000\044\000\000\000\043\000\043\000\030\000\044\000\
\000\000\030\000\043\000\000\000\041\000\000\000\046\000\046\000\
\000\000\000\000\104\000\044\000\046\000\000\000\043\000\054\000\
\055\000\056\000\057\000\058\000\000\000\000\000\000\000\000\000\
\046\000\054\000\055\000\056\000\057\000\058\000"

let yycheck = "\003\000\
\012\000\003\001\002\001\007\001\002\001\045\000\010\000\011\000\
\012\000\013\000\015\001\016\001\010\000\010\001\015\001\016\001\
\001\001\002\001\003\001\004\001\005\001\036\001\037\001\038\001\
\039\001\037\000\023\001\031\001\033\001\031\001\030\001\001\000\
\017\001\037\000\046\000\047\000\032\001\034\001\006\001\043\000\
\044\000\045\000\046\000\047\000\012\000\030\001\044\000\010\001\
\018\001\019\001\020\001\021\001\015\001\016\001\007\001\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\021\001\067\000\
\104\000\105\000\106\000\065\000\072\000\037\000\015\001\016\001\
\026\001\027\001\114\000\043\000\021\001\023\001\046\000\047\000\
\012\000\022\001\001\001\002\001\003\001\004\001\005\001\034\001\
\033\001\008\001\009\001\059\000\060\000\061\000\018\001\019\001\
\020\001\021\001\017\001\031\001\104\000\105\000\106\000\000\000\
\072\000\037\000\025\001\002\001\003\001\028\001\114\000\030\001\
\001\001\002\001\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\066\000\018\001\
\019\001\020\001\021\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\018\001\019\001\020\001\034\001\021\001\003\001\
\035\001\036\001\037\001\038\001\039\001\015\001\016\001\062\000\
\063\000\064\000\033\001\021\001\035\001\036\001\037\001\038\001\
\039\001\011\001\012\001\013\001\014\001\015\001\016\001\033\001\
\018\001\019\001\020\001\021\001\011\001\012\001\013\001\014\001\
\015\001\016\001\001\001\002\001\003\001\004\001\005\001\010\001\
\030\001\035\001\036\001\037\001\038\001\039\001\030\001\021\001\
\029\001\031\001\017\001\031\001\035\001\036\001\037\001\038\001\
\039\001\011\001\012\001\013\001\014\001\015\001\016\001\031\001\
\030\001\021\001\031\001\021\001\011\001\012\001\013\001\014\001\
\015\001\016\001\021\001\021\001\030\001\021\001\021\001\033\001\
\031\001\035\001\015\001\016\001\021\001\069\000\255\255\030\001\
\021\001\255\255\033\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\033\001\021\001\011\001\012\001\
\013\001\014\001\015\001\016\001\015\001\016\001\030\001\255\255\
\021\001\033\001\021\001\011\001\012\001\013\001\014\001\015\001\
\016\001\030\001\015\001\016\001\033\001\021\001\033\001\255\255\
\021\001\015\001\016\001\255\255\015\001\016\001\030\001\021\001\
\255\255\033\001\021\001\255\255\033\001\255\255\015\001\016\001\
\255\255\255\255\030\001\033\001\021\001\255\255\033\001\035\001\
\036\001\037\001\038\001\039\001\255\255\255\255\255\255\255\255\
\033\001\035\001\036\001\037\001\038\001\039\001"

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
# 37 "parser.mly"
                             (_2)
# 310 "parser.ml"
               : ParseTree.tTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
          (TermVar(_1))
# 317 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
            (TermString(_1))
# 324 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 49 "parser.mly"
         (TermBool(_1))
# 331 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 50 "parser.mly"
          (TermBool(_1))
# 338 "parser.ml"
               : 'boolean))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    Obj.repr(
# 54 "parser.mly"
                      (_1)
# 345 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 55 "parser.mly"
                                 ( MultiStatement (_1, _3) )
# 353 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec_op) in
    Obj.repr(
# 59 "parser.mly"
           ( _1 )
# 360 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 60 "parser.mly"
              ( _1 )
# 367 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mut_op) in
    Obj.repr(
# 61 "parser.mly"
           ( _1 )
# 374 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exec_op) in
    Obj.repr(
# 62 "parser.mly"
            ( _1 )
# 381 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 66 "parser.mly"
                                    ( Declaration (_2, _4) )
# 389 "parser.ml"
               : 'dec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 70 "parser.mly"
                   (_1)
# 396 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 71 "parser.mly"
                  (_1)
# 403 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 72 "parser.mly"
                  (_1)
# 410 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 73 "parser.mly"
                  (_1)
# 417 "parser.ml"
               : 'action_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 77 "parser.mly"
                           ( TermMut (_1, _3) )
# 425 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 78 "parser.mly"
                                      ( TermMut(_1, TermConcat (_1, _4)) )
# 433 "parser.ml"
               : 'mut_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action_op) in
    Obj.repr(
# 82 "parser.mly"
                    ( PrintOperation (_2))
# 440 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 83 "parser.mly"
                                                ( ForOperation (_2, _4, _6))
# 449 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'int_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 84 "parser.mly"
                                                        ( ForLoop ( _2, _4, _6))
# 458 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bool_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 85 "parser.mly"
                                                              ( IfStatement (_3, _6) )
# 466 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'bool_operation) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'statements) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 86 "parser.mly"
                                                                                           ( IfElseStatement (_3, _6, _10) )
# 475 "parser.ml"
               : 'exec_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
        (TermInteger(_1))
# 482 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 91 "parser.mly"
          (_1)
# 489 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 92 "parser.mly"
                                     (TermPlus(_1,_3))
# 497 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 93 "parser.mly"
                                      (TermMinus(_1,_3))
# 505 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 94 "parser.mly"
                                      (TermMult(_1,_3))
# 513 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 95 "parser.mly"
                                    (TermDiv(_1,_3))
# 521 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 96 "parser.mly"
                                    (TermMod(_1,_3))
# 529 "parser.ml"
               : 'int_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 100 "parser.mly"
            (_1)
# 536 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 101 "parser.mly"
            (_1)
# 543 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 102 "parser.mly"
                                       (TermConcat (_1, _3) )
# 551 "parser.ml"
               : 'str_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean) in
    Obj.repr(
# 106 "parser.mly"
            (_1)
# 558 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 107 "parser.mly"
          (_1)
# 565 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 108 "parser.mly"
                        (TermNot(_2))
# 572 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 109 "parser.mly"
                                   (TermLt(_1,_3))
# 580 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 110 "parser.mly"
                                    (TermLte(_1,_3))
# 588 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 111 "parser.mly"
                                   (TermGt(_1,_3))
# 596 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 112 "parser.mly"
                                    (TermGte(_1,_3))
# 604 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 113 "parser.mly"
                                   (TermEq(_1,_3))
# 612 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 114 "parser.mly"
                                     (TermEq(_1,_3))
# 620 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 115 "parser.mly"
                                   (TermEq(_1,_3))
# 628 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'int_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_operation) in
    Obj.repr(
# 116 "parser.mly"
                                    (TermNeq(_1,_3))
# 636 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_operation) in
    Obj.repr(
# 117 "parser.mly"
                                      (TermNeq(_1,_3))
# 644 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'str_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 118 "parser.mly"
                                    (TermNeq(_1,_3))
# 652 "parser.ml"
               : 'bool_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "parser.mly"
                (_1 :: [] )
# 659 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 123 "parser.mly"
                           ( _1 :: _3 )
# 667 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 127 "parser.mly"
          ( _1 )
# 674 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
                  ( TermArgs([]) )
# 680 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 129 "parser.mly"
                       (TermArgs( _2 ) )
# 687 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 130 "parser.mly"
                                      ( TermUnion (_1, _3) )
# 695 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 131 "parser.mly"
                                          ( TermIntersection ( _1, _3 ) )
# 703 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_operation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_operation) in
    Obj.repr(
# 132 "parser.mly"
                                     ( TermDifference ( _1, _3 ) )
# 711 "parser.ml"
               : 'set_operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'str_operation) in
    Obj.repr(
# 133 "parser.mly"
                            ( TermAdd (_1, _3) )
# 719 "parser.ml"
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
