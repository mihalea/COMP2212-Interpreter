%{
  open ParseTree
%}

%token <int> INT
%token <string> IDENT
%token PRINT
%token INT_DEC STR_DEC
%token CONCAT
%token SEMICOL
%token QUOTE
%token EOF EOL
%token FOR IN LCURLY RCURLY
%token EQUALS PLUS
%nonassoc PRINT
%left PLUS
%left CONCAT

%start start
%type <ParseTree.tTerm> start

%%

start:
  | LCURLY statements RCURLY EOF {$2};
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
  | INT_DEC ident EQUALS int_operation { IntDeclaration ( $2, $4 ) }
  | STR_DEC ident EQUALS str_operation { StrDeclaration ( $2, $4 ) }
;

action_op:
  | int_operation {$1}
  | str_operation {$1}
;

mut_op:
  | ident EQUALS action_op { TermMut ($1, $3) }
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
