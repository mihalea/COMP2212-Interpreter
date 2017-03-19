%{
  open ParseTree
%}

%token <int> INT
%token <string> IDENT
%token PRINT
%token INT_DEC
%token SEMICOL
%token EOF EOL
%token FOR IN LCURLY RCURLY
%token EQUALS PLUS
%left PLUS

%start start
%type <ParseTree.tTerm> start

%%

start:
  | statements EOF {$1};
;

statements:
  | statement EOL {$1}
  | statement EOL statements { MultiStatement ($1, $3) }
;

statement:
  | dec_op { $1 }
  | action_op { $1 }
  | PRINT IDENT { PrintOperation ($2)}
  | FOR IDENT IN IDENT LCURLY statements RCURLY { ForOperation ($2, $4, $6)}
;

dec_op:
  | INT_DEC IDENT EQUALS int_operation { IntDeclaration( $2, $4)}

action_op:
  | int_operation {$1}
;

int_operation:
  | INT {TermInteger($1)}
  | INT PLUS INT {TermPlus(TermInteger($1), TermInteger($3))}
;
