%{
    open Ast

    let mk_id id startpos endpos =
      { loc = (startpos, endpos); id }
%}

%token <int> NUM
%token <string> IDENT
%token IF ELSE WHILE PRINT
%token LCURLY RCURLY LPAR RPAR
%token SEMICOLON ASSIGN
%token BADD BSUB BMUL BDIV BMOD
%token EOF

%left SEMICOLON

%left BADD BSUB
%left BMUL BDIV BMOD

%start file
%type <Ast.file> file

%%

file:
| s = stmt EOF { s }
;

stmt:
| /* skip */ { Sskip }
| IF LPAR e = expr RPAR LCURLY s1 = stmt RCURLY
  ELSE LCURLY s2 = stmt RCURLY
    { Sif (e, s1, s2) }
| WHILE LPAR e = expr RPAR LCURLY s = stmt RCURLY
    { Swhile (e, s) }
| s1 = stmt SEMICOLON s2 = stmt
    { Sseq (s1, s2) }
| x = ident ASSIGN e = expr
    { Sassign (x, e) }
| PRINT LPAR e = expr RPAR
    { Sprint e }
;

expr:
| n = NUM { Ecst n }
| x = ident { Evar x }
| e1 = expr o = op e2 = expr { Ebinop (o, e1, e2) }
;

%inline op:
| BADD { Badd }
| BSUB { Bsub }
| BMUL { Bmul }
| BDIV { Bdiv }
| BMOD { Bmod }
;

ident:
| x = IDENT { mk_id x $startpos $endpos }
