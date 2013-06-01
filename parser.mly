%{
open Exp
%}

%token <int> INT
%token <bool> BOOL
%token <string> VARIABLE
%token LPAREN
%token RPAREN
%token EQUAL
%token LESS
%token MINUS_GREATER
%token PLUS
%token MINUS
%token AST
%token EOI
%token THEN
%token ELSE
%token LET
%token FUN
%token REC
%token IN
%token IF
%token QUIT

%nonassoc IN
%nonassoc LET
%nonassoc THEN
%nonassoc ELSE
%nonassoc LESS
%left PLUS MINUS
%left AST
%left prec_app

%type <Toplevel.t> toplevel
%start toplevel

%%

toplevel:
  | exp EOI
      { Toplevel.Exp ($1) }
  | QUIT EOI
      { Toplevel.Quit }

exp:
  | simple_exp
      { $1 }
  | exp PLUS exp
      { BinOp ($1, Prim.Plus, $3) }
  | exp MINUS exp
      { BinOp ($1, Prim.Minus, $3) }
  | exp AST exp
      { BinOp ($1, Prim.Times, $3) }
  | exp LESS exp
      { BinOp ($1, Prim.Lt, $3) }
  | IF exp THEN exp ELSE exp
      { If ($2, $4, $6) }
  | LET VARIABLE EQUAL exp IN exp
      { Let ($2, $4, $6) }
  | FUN VARIABLE MINUS_GREATER exp
      { Fun ($2, $4) }
  | exp simple_exp 
      %prec prec_app
      { App ($1, $2) }
  | LET REC VARIABLE EQUAL FUN VARIABLE MINUS_GREATER exp IN exp
      { LetRec ($3, $6, $8, $10) }

simple_exp:
  | INT
      { Int ($1) }
  | BOOL
      { Bool ($1) }
  | VARIABLE
      { Var ($1) }
  | LPAREN exp RPAREN
      { $2 }
