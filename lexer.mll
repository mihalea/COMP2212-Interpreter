{
    open Parser
}
let keywords =  [
    "for"; "if"; "else"; "index"; "subset"; "in"; "read"; "print"; "var"
];
rule token = parse
      [' ', '\t']                               { token lexbuf }
    | ['\n']                                    { EOL }
    | ['0'-'9']+ as num                         { INT (int_of_string num) }
    | ['a'-'z']+                                { LITERAL }
    | ['a'-'z']['a'-'z''0'-'9''_']+ as i        { 
                                                    try List.assoc i keywords
                                                    with Not_found -> IDENT
                                                }
    | "var"                                     { VAR }
    | "for"                                     { FOR }
    | "if"                                      { IF }
    | "else"                                    { ELSE }
    | '='                                       { ASSIGN }
    | "read"                                    { READ }
    | "print"                                   { PRINT }
    | eof                                       { EOF }
    | '{'                                       { LCURLY }
    | '}'                                       { RCURLY }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | '"'                                       { QUOTE }
    | ','                                       { COMMA }
    | '+'                                       { PLUS }
    | '-'                                       { MINUS }
    | '*'                                       { TIMES }
    | '/'                                       { DIV }
    | ':'                                       { EMPTY }
    | '.'                                       { CONCAT }
    | '!'                                       { KLEENE }
    | "index"                                   { INDEX }
    | "len"                                     { LENGTH }
    | '|'                                       { UNION }
    | '&'                                       { INTERSECT }
    | '#'                                       { SUBTR }
    | '%'                                       { CARTESIAN }
    | "in"                                      { BELONG }
    | "subset"                                  { SUBSET }
    | '<'                                       { LT }
    | '>'                                       { GT }
    | "<="                                      { LEQ }
    | ">="                                      { GEQ }
    | "=="                                      { EQ }
    | "!="                                      { NEQ }
