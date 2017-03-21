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

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ParseTree.tTerm
