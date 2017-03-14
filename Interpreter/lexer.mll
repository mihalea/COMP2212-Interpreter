{
    open Parser
}
let keywords =  [
    "for"; "if"; "else"; "subset"; "in"; "print"; "var"
];
rule token = parse
      [' ', '\t']                               { token lexbuf }
    | ['\n']                                    { EOL }
    | "var"                                     { VAR }
    | "for"                                     { FOR }
    | "if"                                      { IF }
    | "else"                                    { ELSE }
    | "print"                                   { PRINT }
    | "len"                                     { LENGTH }
    | "in"                                      { BELONG }
    | "subset"                                  { SUBSET }
    | ['0'-'9']+ as num                         { INT (int_of_string num) }
    | '"'['a'-'z']+'"'                                { LITERAL }
    | ['a'-'z']['a'-'z''0'-'9''_']+ as i        {
                                                    try List.assoc i keywords
                                                    with Not_found -> IDENT
                                                }
    | '='                                       { ASSIGN }
    | eof                                       { EOF }
    | '{'                                       { LCURLY }
    | '}'                                       { RCURLY }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | ','                                       { COMMA }
    | '+'                                       { PLUS }
    | '-'                                       { MINUS }
    | '*'                                       { TIMES }
    | '/'                                       { DIV }
    | '%'                                       { MOD }
    | ':'                                       { EMPTY }
    | '^'                                       { CONCAT }
    | '~'                                       { ADD }
    | '!'                                       { KLEENE }
    | '|'                                       { UNION }
    | '&'                                       { INTERSECT }
    | '#'                                       { SUBTR }
    | '@'                                       { CARTESIAN }
    | '<'                                       { LT }
    | '>'                                       { GT }
    | "<="                                      { LEQ }
    | ">="                                      { GEQ }
    | "=="                                      { EQ }
    | "!="                                      { NEQ }
