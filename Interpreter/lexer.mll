{

open Parser

}

rule next = parse
  | ['0'-'9']+ as id { INT (int_of_string id) }
  | ';' {SEMICOL}
  | '\t' | ' ' | '\n' { next lexbuf }
  | '+' { PLUS }
  | "int" { INT_DEC }
  | "print" { PRINT }
  | '=' { EQUALS }
  | '{' { LCURLY }
  | '}' {RCURLY}
  | "for" {FOR}
  | "in" {IN}
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT id }
  | eof { EOF }
