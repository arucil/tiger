%{

open Lex.Token

%}

%token <token_pos> WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY IF THEN
%token <token_pos> ELSE DO OF NIL
%token <token_pos> COMMA COLON SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token <token_pos> PLUS MINUS MULT DIV EQ NEQ GT GE LT LE AND OR DOT ASSIGN
%token <token_pos> EOF
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

%type <unit> prog expr

%start prog

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
  | STR {}
  | INT {}
  | NIL {}
  | LPAREN; separated_nonempty_list(SEMI, expr); RPAREN {}
  | LPAREN; RPAREN {}
  | LET; list(decl); IN; separated_list(SEMI, expr); END {}
  | IF; expr; THEN; expr; %prec IFX {}
  | IF; expr; THEN; expr; ELSE; expr {}
  | WHILE; expr; DO; expr {}
  | FOR; ID; ASSIGN; expr; TO; expr; DO; expr {}
  | BREAK {}
  | ID; LBRACE; separated_list(COMMA, separated_pair(ID, EQ, expr)); RBRACE {}
  | ID; LPAREN; separated_list(COMMA, expr); RPAREN {}
  | lvalue; ASSIGN; expr {}
  | lvalue {}
  | ID; LBRACK; expr; RBRACK; OF; expr {}
  ;

lvalue:
  | ID; list(lvalue_suffix) {}
  ;

lvalue_suffix:
  | DOT; ID {}
  | LBRACK; expr; RBRACK {}
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
  ;