%{

open Lex.Token

%}

%token <token_pos> WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY IF THEN
%token <token_pos> ELSE DO OF NIL
%token <token_pos> COMMA COLON SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token <token_pos> DOT PLUS MINUS MULT DIV EQ NEQ GT GE LT LE AND OR ASSIGN EOF
%token <token_pos * string> ID STR
%token <token_pos * int> INT

%nonassoc DO
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ GT GE LT LE
%left PLUS MINUS
%left MULT DIV
%nonassoc UMINUS
%nonassoc OF
%nonassoc IFX
%nonassoc ELSE

%start <unit> prog expr

%%

prog:
  | expr; EOF {}
  ;

expr:
  | expr; OR; expr {}
  | expr; AND; expr {}
  | expr; EQ; expr {}
  | expr; NEQ; expr {}
  | expr; GT; expr {}
  | expr; GE; expr {}
  | expr; LT; expr {}
  | expr; LE; expr {}
  | expr; PLUS; expr {}
  | expr; MINUS; expr {}
  | expr; MULT; expr {}
  | expr; DIV; expr {}
  | MINUS; expr %prec UMINUS {}
  | lvalue; DOT; ID {}
  | array_expr {}
  | ID; LBRACE; separated_list(COMMA, separated_pair(ID, EQ, expr)); RBRACE {}
  | lvalue; ASSIGN; expr {}
  | IF; expr; THEN; expr; %prec IFX {}
  | IF; expr; THEN; expr; ELSE; expr {}
  | WHILE; expr; DO; expr {}
  | FOR; ID; ASSIGN; expr; TO; expr; DO; expr {}
  | BREAK {}
  | STR {}
  | INT {}
  | NIL {}
  | LPAREN; separated_nonempty_list(SEMI, expr); RPAREN {}
  | LPAREN; RPAREN {}
  | ID; LPAREN; separated_list(COMMA, expr); RPAREN {}
  | LET; list(decl); IN; separated_list(SEMI, expr); END {}
  ;

lvalue:
  | ID {}
  | lvalue; DOT; ID {}
  | lvalue; LBRACK; expr; RBRACK {}
  ;

array_expr:
  | ID; LBRACK; expr; RBRACK; option(array_length) {}
  ;

array_length:
  | OF; expr {}
  ;

decl:
  | type_decl {}
  | var_decl {}
  | fun_decl {}
  ;

type_decl:
  | TYPE; ID; EQ; ty {}
  ;

var_decl:
  | VAR; ID; ASSIGN; expr {}
  | VAR; ID; COLON; ID; ASSIGN; expr {}
  ;

fun_decl:
  | FUNCTION; ID; LPAREN; fields; RPAREN; EQ; expr {}
  | FUNCTION; ID; LPAREN; fields; RPAREN; COLON; ID; EQ; expr {}
  ;

ty:
  | ID {}
  | LBRACE; fields; RBRACE {}
  | ARRAY; OF; ID {}
  ;

fields:
  | separated_list(COMMA, separated_pair(ID, COLON, ID)) {}