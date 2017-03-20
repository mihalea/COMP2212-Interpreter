%{
  open ParseTree
%}

%token <int> INT
%token <string> IDENT
%token BEGIN END
%token PRINT
%token VAR_DEC
%token CONCAT
%token UNION
%token SEMICOL, COMMA
%token QUOTE
%token EOF EOL
%token FOR IN LCURLY RCURLY
%token EQUALS PLUS
%nonassoc PRINT
%left PLUS CONCAT UNION

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
  | QUOTE IDENT QUOTE {TermString($2)}

statements:
  | statement SEMICOL {$1}
  | statement SEMICOL statements { MultiStatement ($1, $3) }
;

statement:
  | dec_op { $1 }
  | action_op { $1 }
  | mut_op { $1 }
  | PRINT action_op { PrintOperation ($2)}
  | FOR ident IN ident LCURLY statements RCURLY { ForOperation ($2, $4, $6)}
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
    | QUOTE IDENT QUOTE {$2 :: [] }
    | QUOTE IDENT QUOTE COMMA args { $2 :: $5 }
;

set_operation:
  | ident { $1 }
  | LCURLY RCURLY { TermArgs([]) }
  | LCURLY args RCURLY {TermArgs( $2 ) } 
  | set_operation UNION set_operation { TermUnion ($1, $3) }
;
