{

open Parser

}

rule next = parse
  | ['0'-'9']+ as id { INT (int_of_string id) }
  | ';' {SEMICOL}
  | '\t' | ' ' | '\n' { next lexbuf }
  | '+' { PLUS }
  | "begin" {BEGIN}
  | "end" {END}
  | "var" { VAR_DEC }
  | "print" { PRINT }
  | '^' {CONCAT}
  | "union" {UNION}
  | "intersect" {INTERSECT}
  | "diff" { DIFF }
  | '=' { EQUALS }
  | '{' { LCURLY }
  | '}' {RCURLY}
  | '"' {QUOTE}
  | ',' {COMMA}
  | "for" {FOR}
  | "in" {IN}
  | ['a'-'z''A'-'Z''0'-'9']+ as id { IDENT id }
  | eof { EOF }
