%{
  open Ast
%}

%token LET IN IF THEN ELSE FUN EQ
%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token REC
%token EOF LPAREN RPAREN COMMA 
%token RIGHT_ARROW

%nonassoc LET REC
%nonassoc IN
%nonassoc IF               /* lowest precedence */
%right RIGHT_ARROW
%right COMMA
%nonassoc IDENT LPAREN RPAREN  /* highest precedence */        

%start prog
%type <Ast.expr> expression
%type <Ast.pat> pat
%type <Ast.expr> prog


%%

prog :
  | expression EOF { $1 }
;

expression :
  | LPAREN expression RPAREN                      { $2 }
  | IDENT                                         { Ident($1) }
  | INT                                           { Number($1) }
  | FALSE                                         { False }
  | TRUE                                          { True }
  | FUN pat RIGHT_ARROW expression                { Lambda($2,$4) }
  | LET pat EQ expression IN expression           { Let($2,$4,$6) }
  | LET REC pat EQ expression IN expression       { LetRec($3,$5,$7) }
  | IF expression THEN expression ELSE expression { If($2,$4,$6) }
  | expression expression                         { Apply($1,$2) }
  | LPAREN expression COMMA expression RPAREN     { Mlpair($2,$4) }
;

pat:
  | LPAREN pat RPAREN                             { $2 }
  | IDENT                                         { IdentPat($1) }
  | LPAREN RPAREN                                 { NullPat } 
  | LPAREN pat COMMA pat RPAREN                   { Pairpat($2,$4) }
;
