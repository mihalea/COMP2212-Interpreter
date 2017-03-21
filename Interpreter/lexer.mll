{

open Parser

}

rule next = parse
  | ['0'-'9']+ as id { INT (int_of_string id) }
  | "true" { TRUE true }
  | "false" { FALSE false }
  | ';' {SEMICOL}
  | '\t' | ' ' | '\n' { next lexbuf }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '%' { MOD }
  | "begin" {BEGIN}
  | "end" {END}
  | "var" { VAR_DEC }
  | "print" { PRINT }
  | '^' {CONCAT}
  | "union" {UNION}
  | "intersect" {INTERSECT}
  | "diff" { DIFF }
  | "add" { ADD }
  | '=' { EQUALS }
  | '<' { LT }
  | '{' { LCURLY }
  | '}' {RCURLY}
  | '(' {LPAREN}
  | ')' {RPAREN}
  | ',' {COMMA}
  | "if" {IF}
  | "else" {ELSE}
  | "for" {FOR}
  | "in" {IN}
  | "to" {TO}
  | '"'[':''a'-'z''A'-'Z''0'-'9']+'"' as lxm { LITERAL (List.nth (Str.split_delim (Str.regexp "\"") lxm) 1) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT id }
  | eof { EOF }
