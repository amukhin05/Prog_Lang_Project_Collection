%{
open Utils
%}

%token IF
%token THEN
%token ELSE
%token LET
%token EQ
%token IN
%token FUN 
%token APP
%token LPAREN
%token RPAREN
%token TRUE
%token FALSE
%token PLUS
%token MINUS
%token MUL
%token DIV
%token MOD
%token LT 
%token LTE 
%token GT 
%token GTE 
%token AND
%token OR
%token NEQ
%token LBRACE
%token RBRACE
%token<int> INT 
%token<string> IDENT
%token EOF

%start <Utils.prog> prog

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left PLUS MINUS
%left MUL DIV MOD

%%

prog:
  | e = expr EOF { e }
  | EOF { Unit }
;

expr:
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { If(e1, e2, e3) }
  | LET v = var EQ e1 = expr IN e2 = expr { Let(v, e1, e2) }
  | FUN v = var APP e = expr { Fun(v, e) }
  | e = expr2 { e }
;

expr2:
  | e1 = expr2 b = bop e2 = expr2 { Bop(b, e1, e2) }
  | e1 = expr3 LBRACE e2 = expr3 RBRACE { App(e1, e2) }
;

expr3:
  | LPAREN RPAREN { Unit }
  | TRUE { True }
  | FALSE { False }
  | n = INT { Num n }
  | v = IDENT { Var v }
  | LPAREN e = expr RPAREN { e }
;

%inline bop:
  | PLUS { Add }
  | MINUS { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | AND { And }
  | OR { Or }
  | NEQ { Neq }
;

var: 
  | x = IDENT { x }
;

int:
  | n = INT { n }
;




