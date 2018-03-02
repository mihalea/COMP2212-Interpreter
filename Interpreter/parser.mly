%{
  open ParseTree
%}

%token <int> INT
%token <string> IDENT
%token <string> LITERAL
%token BEGIN END
%token PRINT
%token VAR_DEC
%token CONCAT
%token UNION INTERSECT DIFF
%token SEMICOL, COMMA
%token ADD
%token EOF EOL
%token FOR TO IN LCURLY RCURLY
%token EQUALS PLUS
%nonassoc PRINT
%left ADD
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
;

mut_op:
  | ident EQUALS action_op { TermMut ($1, $3) }
  | ident CONCAT EQUALS str_operation { TermMut($1, TermConcat ($1, $4)) }
;

exec_op:
  | PRINT action_op { PrintOperation ($2)}
  | FOR ident IN ident LCURLY statements RCURLY { ForOperation ($2, $4, $6)}
  | FOR ident TO int_operation LCURLY statements RCURLY { ForLoop ( $2, $4, $6)}
;

int_operation:
  | INT {TermInteger($1)}
  | ident {$1}
  | int_operation PLUS int_operation {TermPlus($1,$3)}
;

str_operation:
  | literal {$1}
  | ident   {$1}
  | str_operation CONCAT str_operation {TermConcat ($1, $3) }
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
