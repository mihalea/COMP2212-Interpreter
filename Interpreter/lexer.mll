{

open Parser

}

rule next = parse
  | ['0'-'9']+ as id { INT (int_of_string id) }
  | '\n' { EOL }
  | '\t' | ' ' { next lexbuf }
  | '+' { PLUS }
  | eof { EOF }
