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

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ParseTree.tTerm
