%token <int> INT
%token <string> LITERAL
%token <string> IDENT
%token VAR FOR IF ELSE
%token ASSIGN
%token READ PRINT
%token EOF EOL
%token LCURLY RCURLY LPAREN RPAREN QUOTE COMMA
%token PLUS MINUS TIMES DIV
%token EMPTY
%token KLEENE LENGTH INDEX
%token CONCAT UNION INTERSECT SUBTR CARTESIAN SUBSET
%token BELONG
%token LT GT LEQ GEQ EQ NEQ
%start main
%type <int> main
%%
main:
    statements EOF                  { raise End_of_file }
;
statements:
    | { }
    | statement EOL statements      {   }
;
statement:
      INT                           { $1 }
    | LITERAL                       { $1 }
    | LPAREN expr RPAREN            { $2 }
    | IDENT                         { /* TODO */ }
    | VAR IDENT ASSIGN expr         { /* TODO */ }
    | IF LPAREN bool_expr RPAREN
        LCURLY statemets RCURLY     { }
    | IF LPAREN bool_expr RPAREN
            LCURLY statements RCURLY
        ELSE
            LCURLY statements RCURLY { }
    | READ IDENT                    { }
    | PRINT IDENT                   { }
    |  
;
expr:
    | int_expr
;
int_expr:
    | INT                           { $1 }
    | LPAREN int_expr RPAREN        { $2 }
    | int_expr PLUS int_expr        { Plus ($1,$3) }
    | int_expr MINUS int_expr        { Minus ($1,$3) }
    | int_expr TIMES int_expr        { Times ($1,$3) }
    | int_expr DIV int_expr        { Times ($1,$3) }
    | int_expr MOD int_expr        { Mod ($1,$3) }
;
