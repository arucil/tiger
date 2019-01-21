%{

open Lex.Token
open Base

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

(* Workaround for Printf.fprintf not included in `base' *)
module Printf = struct
  include Printf
  let fprintf = Stdio.Out_channel.fprintf
end

let rec merge_decl_group : Ast.decl list -> Ast.decl list = function
  | decl1 :: decl2 :: decls ->
      (match decl1, decl2 with
         | FunDecl xs, FunDecl ys -> merge_decl_group (Ast.FunDecl (xs @ ys) :: decls)
         | TypeDecl xs, TypeDecl ys -> merge_decl_group (Ast.TypeDecl (xs @ ys) :: decls)
         | _ -> decl1 :: merge_decl_group (decl2 :: decls))
  | decls -> decls

%}

%token <token_pos> WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY IF THEN
%token <token_pos> ELSE DO OF NIL
%token <token_pos> COMMA COLON SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token <token_pos> PLUS MINUS TIMES DIV EQ NEQ GT GE LT LE AND OR DOT ASSIGN
%token <token_pos> EOF
%token <token_pos * string> ID STR
%token <token_pos * int> INT

%nonassoc DO
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ GT GE LT LE
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%nonassoc OF
%nonassoc IFX
%nonassoc ELSE

%type <Ast.expr> prog expr
%type <Ast.var> lvalue
%type <Ast.ty> ty
%type <Ast.decl> fun_decl type_decl var_decl
%type <Ast.field list> fields
%type <Ast.param list> params

%start prog

%%

prog:
  | e = expr; EOF
    { e }
  ;

expr:
  | lhs = expr; pos = OR; rhs = expr      { (pos, BinaryExpr { lhs; rhs; op = OrOp }) }
  | lhs = expr; pos = AND; rhs = expr     { (pos, BinaryExpr { lhs; rhs; op = AndOp }) }
  | lhs = expr; pos = EQ; rhs = expr      { (pos, BinaryExpr { lhs; rhs; op = EqOp }) }
  | lhs = expr; pos = NEQ; rhs = expr     { (pos, BinaryExpr { lhs; rhs; op = NeqOp }) }
  | lhs = expr; pos = GT; rhs = expr      { (pos, BinaryExpr { lhs; rhs; op = GtOp }) }
  | lhs = expr; pos = GE; rhs = expr      { (pos, BinaryExpr { lhs; rhs; op = GeOp }) }
  | lhs = expr; pos = LT; rhs = expr      { (pos, BinaryExpr { lhs; rhs; op = LtOp }) }
  | lhs = expr; pos = LE; rhs = expr      { (pos, BinaryExpr { lhs; rhs; op = LeOp }) }
  | lhs = expr; pos = PLUS; rhs = expr    { (pos, BinaryExpr { lhs; rhs; op = AddOp }) }
  | lhs = expr; pos = MINUS; rhs = expr   { (pos, BinaryExpr { lhs; rhs; op = SubOp }) }
  | lhs = expr; pos = TIMES; rhs = expr   { (pos, BinaryExpr { lhs; rhs; op = MulOp }) }
  | lhs = expr; pos = DIV; rhs = expr     { (pos, BinaryExpr { lhs; rhs; op = DivOp }) }
  | pos = MINUS; rand = expr %prec UMINUS { (pos, UnaryExpr { op = NegOp; rand }) }
  | v = STR { let (pos, s) = v in (pos, StrExpr s) }
  | v = INT { let (pos, n) = v in (pos, IntExpr n) }
  | pos = NIL     { (pos, NilExpr) }
  | pos = LPAREN; exprs = separated_list(SEMI, expr); RPAREN
    {
      match exprs with
      | [expr] -> expr
      | _ -> (pos, SeqExpr exprs)
    }
  | pos = LET; decls = decl*; body_pos = IN; exprs = separated_list(SEMI, expr); END
    {
      (pos,
       LetExpr {
         decls = merge_decl_group decls;
         body =
           match exprs with
           | [expr] -> expr
           | _ -> (body_pos, SeqExpr exprs)
       })
    }
  | pos = IF; cond = expr; THEN; conseq = expr; %prec IFX
    {
      (pos, IfExpr { cond; conseq; alt = None })
    }
  | pos = IF; cond = expr; THEN; conseq = expr; ELSE; alt = expr
    {
      (pos, IfExpr { cond; conseq; alt = Some alt })
    }
  | pos = WHILE; cond = expr; DO; body = expr
    {
      (pos, WhileExpr { cond; body })
    }
  | pos = FOR; var_v = ID; ASSIGN; low = expr; TO; high = expr; DO; body = expr
    {
      (pos, ForExpr { var = snd var_v; escape = ref true; low; high; body })
    }
  | pos = BREAK
    {
      (pos, BreakExpr)
    }
  | ty_v = ID; LBRACE; fields = separated_list(COMMA, field); RBRACE
    {
      let (pos, ty) = ty_v in
      (pos, RecordExpr { fields; ty })
    }
  | func_v = ID; LPAREN; args = separated_list(COMMA, expr); RPAREN
    {
      let (pos, func) = func_v in
      (pos, CallExpr { func; args })
    }
  | var = lvalue; pos = ASSIGN; expr = expr
    {
      (pos, AssignExpr { var; expr })
    }
  | var = lvalue
    {
      match var with
      | SimpleVar (pos, _) -> (pos, VarExpr var)
      | FieldVar (pos, _, _) -> (pos, VarExpr var)
      | IndexVar (pos, _, _) -> (pos, VarExpr var)
    }
  | ty_v = ID; LBRACK; size = expr; RBRACK; OF; init = expr
    {
      let (pos, ty) = ty_v in
      (pos, ArrayExpr { ty; size; init })
    }
  ;

field:
  | var_v = ID; EQ; expr = expr;
    { let (pos, var) = var_v in (pos, var, expr) }
  ;

lvalue:
  | var_v = ID; xs = lvalue_suffix*
    {
      let (pos, var) = var_v in
      List.fold xs ~init:(SimpleVar (pos, var) : Ast.var)
        ~f:(fun var -> function
          | Left (pos, id) -> FieldVar (pos, var, id)
          | Right (pos, expr) -> IndexVar (pos, var, expr))
    }
  ;

lvalue_suffix:
  | DOT; id_v = ID
    { Left id_v }
  | pos = LBRACK; expr = expr; RBRACK
    { Right (pos, expr) }
  ;

decl:
  | d = type_decl { d }
  | d = var_decl { d }
  | d = fun_decl { d }
  ;

type_decl:
  | pos = TYPE; name_v = ID; EQ; ty = ty
    {
      let (_, name) = name_v in
      TypeDecl [{ name; ty; pos }]
    }
  ;

var_decl:
  | pos = VAR; name_v = ID; ASSIGN; init = expr
    {
      let (_, name) = name_v in
      VarDecl { name; escape = ref true; ty = None; init; pos }
    }
  | pos = VAR; name_v = ID; COLON; ty_v = ID; ASSIGN; init = expr
    {
      let (_, name) = name_v in
      VarDecl { name; escape = ref true; ty = Some ty_v; init; pos }
    }
  ;

fun_decl:
  | pos = FUNCTION; name_v = ID; LPAREN; params = params; RPAREN; EQ; body = expr
    {
      let (_, name) = name_v in
      FunDecl [{ name; params; ret = None; body; pos }]
    }
  | pos = FUNCTION; name_v = ID; LPAREN; params = params; RPAREN; COLON; ret_v = ID; EQ; body = expr
    {
      let (_, name) = name_v in
      FunDecl [{ name; params; ret = Some ret_v; body; pos }]
    }
  ;

ty:
  | id_v = ID
    { NameType id_v }
  | LBRACE; fields = fields; RBRACE
    { RecordType fields }
  | pos = ARRAY; OF; id_v = ID
    {
      let (_, id) = id_v in
      ArrayType (pos, id)
    }
  ;

fields:
  | fields = separated_list(COMMA, separated_pair(ID, COLON, ID))
    {
      List.map fields ~f:(fun ((pos, name), (_, ty)) ->
        ({ name; ty; pos } : Ast.field))
    }
  ;

params:
  | params = separated_list(COMMA, separated_pair(ID, COLON, ID))
    {
      List.map params ~f:(fun ((pos, name), (_, ty)) ->
        ({ name; escape = ref true; ty; pos } : Ast.param))
    }
  ;