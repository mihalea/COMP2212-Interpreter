%{%}

%token <int> INT
%token SEMICOL
%token EOF EOL
%token PLUS
%left PLUS

%start start
%type <int> start

%%

start:
  | statements EOF {$1};
;

statements:
  | statement EOL {$1}
  | statement EOL statements {$1}
;

statement:
  | INT {$1}
  | INT PLUS INT {$1 + $3}
;
