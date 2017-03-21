%{
  open ParseTree
%}

%token <int> INT
%token <string> IDENT
%token <string> LITERAL
%token <bool> TRUE
%token <bool> FALSE 
%token BEGIN END
%token PRINT
%token VAR_DEC
%token CONCAT
%token LT
%token UNION INTERSECT DIFF
%token SEMICOL, COMMA
%token ADD
%token EOF EOL
%token FOR TO IN 
%token IF ELSE
%token LCURLY RCURLY LPAREN RPAREN
%token EQUALS PLUS MINUS TIMES DIV MOD
%nonassoc PRINT
%left ADD
%left LT
%left PLUS CONCAT UNION INTERSECT DIFF

%start start
%type <ParseTree.tTerm> start

%%

start:
  | BEGIN statements END EOF {$2};
;

ident:
  | IDENT {TermVar($1)}
;

literal:
  | LITERAL {TermString($1)}
;

boolean: 
  | TRUE {TermBool($1)}
  | FALSE {TermBool($1)}
;

statements:
  | statement SEMICOL {$1}
  | statement SEMICOL statements { MultiStatement ($1, $3) }
;

statement:
  | dec_op { $1 }
  | action_op { $1 }
  | mut_op { $1 }
  | exec_op { $1 }
;

dec_op:
  | VAR_DEC ident EQUALS action_op  { Declaration ($2, $4) }
;

action_op:
  | int_operation {$1}
  | str_operation {$1}
  | set_operation {$1}
  | bool_opreration {$1}
;

mut_op:
  | ident EQUALS action_op { TermMut ($1, $3) }
  | ident CONCAT EQUALS str_operation { TermMut($1, TermConcat ($1, $4)) }
;

exec_op:
  | PRINT action_op { PrintOperation ($2)}
  | FOR ident IN ident LCURLY statements RCURLY { ForOperation ($2, $4, $6)}
  | FOR ident TO int_operation LCURLY statements RCURLY { ForLoop ( $2, $4, $6)}
  | IF LPAREN bool_opreration RPAREN LCURLY statements RCURLY  { IfStatement ($3, $6) }
  | IF LPAREN bool_opreration RPAREN LCURLY statements RCURLY ELSE LCURLY statements RCURLY { IfElseStatement ($3, $6, $10) }
;

int_operation:
  | INT {TermInteger($1)}
  | ident {$1}
  | int_operation PLUS int_operation {TermPlus($1,$3)}
  | int_operation MINUS int_operation {TermMinus($1,$3)}
  | int_operation TIMES int_operation {TermMult($1,$3)}
  | int_operation DIV int_operation {TermDiv($1,$3)}
  | int_operation MOD int_operation {TermMod($1,$3)}
;

str_operation:
  | literal {$1}
  | ident   {$1}
  | str_operation CONCAT str_operation {TermConcat ($1, $3) }
;

bool_opreration:
  | boolean {$1}
  | ident {$1}
  | int_operation LT int_operation {TermLt($1,$3)}
;

args:
    |  LITERAL  {$1 :: [] }
    |  LITERAL  COMMA args { $1 :: $3 }
;

set_operation:
  | ident { $1 }
  | LCURLY RCURLY { TermArgs([]) }
  | LCURLY args RCURLY {TermArgs( $2 ) }
  | set_operation UNION set_operation { TermUnion ($1, $3) }
  | set_operation INTERSECT set_operation { TermIntersection ( $1, $3 ) }
  | set_operation DIFF set_operation { TermDifference ( $1, $3 ) }
  | ident ADD str_operation { TermAdd ($1, $3) }
;
