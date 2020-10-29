open Base
open Common

type entry = int * bool ref

let rec feexpr (env : entry Symtab.t) depth (_, (expr : Ast.expr')) : unit =
  match expr with
  | VarExpr var -> fevar env depth var
  | NilExpr | IntExpr _ | StrExpr _ -> ()
  | CallExpr { args; _ } -> List.iter args ~f:(feexpr env depth)
  | BinaryExpr { lhs; rhs; _ } ->
    (feexpr env depth lhs;
     feexpr env depth rhs)
  | UnaryExpr { rand; _ } ->
    feexpr env depth rand
  | RecordExpr { fields; _ } ->
    List.iter fields ~f:(fun (_, _, expr) -> feexpr env depth expr)
  | ArrayExpr { size; init; _ } ->
    (feexpr env depth size;
     feexpr env depth init)
  | SeqExpr exprs ->
    List.iter exprs ~f:(feexpr env depth)
  | AssignExpr { var; expr } ->
    (fevar env depth var;
     feexpr env depth expr)
  | IfExpr { cond; conseq; alt } ->
    (feexpr env depth cond;
     feexpr env depth conseq;
     Option.iter alt ~f:(feexpr env depth))
  | WhileExpr { cond; body } ->
    (feexpr env depth cond;
     feexpr env depth body)
  | ForExpr { var; escape; low; high; body } ->
    begin
      feexpr env depth low;
      feexpr env depth high;
      escape := false;
      feexpr (Symtab.extend env var (depth, escape)) depth body
    end
  | BreakExpr -> ()
  | LetExpr { decls; body } ->
    feexpr (fedecls env depth decls) depth body

and fevar env depth (var : Ast.var) : unit =
  match var with
  | SimpleVar (_, sym) ->
    Option.iter (Symtab.find env sym)
      ~f:(fun (d, esc) ->
        if depth > d then
          esc := true)
  | FieldVar (_, var, _) -> fevar env depth var
  | IndexVar (_, var, expr) ->
    (fevar env depth var;
     feexpr env depth expr)

and fedecls env depth = List.fold ~init:env ~f:(fedecl depth)

and fedecl depth env = function
  | VarDecl { name; escape; init; _ } ->
    begin
      feexpr env depth init;
      escape := false;
      Symtab.extend env name (depth, escape)
    end
  | TypeDecl _ -> env
  | FunDecl fundecls ->
    (fefundecls (depth + 1) env fundecls;
    env)

and fefundecls depth env fundecls =
  List.iter fundecls ~f:(fun { params; body; _ } ->
    let env = List.fold params ~init:env
      ~f:(fun env ({ name; escape; _ } : Ast.param) ->
        escape := false;
        Symtab.extend env name (depth, escape))
    in
      feexpr env depth body)

let find_escape expr = feexpr Symtab.empty 0 expr