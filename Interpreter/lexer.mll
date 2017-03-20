{

open Parser

}

rule next = parse
  | ['0'-'9']+ as id { INT (int_of_string id) }
  | ';' {SEMICOL}
  | '\t' | ' ' | '\n' { next lexbuf }
  | '+' { PLUS }
  | "var" { VAR_DEC }
  | "print" { PRINT }
  | '^' {CONCAT}
  | "union" {UNION}
  | '=' { EQUALS }
  | '{' { LCURLY }
  | '}' {RCURLY}
  | '"' {QUOTE}
  | ',' {COMMA}
  | "for" {FOR}
  | "in" {IN}
  | ['a'-'z''A'-'Z''0'-'9']+ as id { IDENT id }
  | eof { EOF }
