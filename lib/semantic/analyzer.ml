open Base
open Parse
open Ast
open Poly

let check_int pos ty =
  if ty <> Type.IntType then
    Errors.report pos "expected %s type, got %s type"
      (Type.show Type.IntType) (Type.show ty)

let check_eq_type pos ty =
  if ty <> Type.IntType && ty <> Type.StringType && ty <> Type.NilType
      && not (Type.is_record ty) && not (Type.is_array ty) then
    Errors.report pos "expected int, string, nil, record or array type, got %s type"
      (Type.show ty)

let report_undef_var pos sym =
  Errors.report pos "undefined variable: %s" (Symbol.name sym)

let rec iter2 f xs ys =
  match xs, ys with
  | (x :: xs'), (y :: ys') ->
    f x y;
    iter2 f xs' ys'
  | _ -> ()

let iter2i f xs ys =
  let rec go i xs ys =
    match xs, ys with
    | (x :: xs'), (y :: ys') ->
      f i x y;
      go (i + 1) xs' ys'
    | _ -> ()
  in go 0 xs ys

let check_field (field_sym, field_ty) (pos, actual_sym, actual_ty) =
  let field_ty = Type.actual_type field_ty in
  if field_sym <> actual_sym || not (Type.is_compatible field_ty actual_ty) then
    Errors.report pos "expected field '%s' of %s type, got field '%s' of %s type"
      (Symbol.name field_sym) (Type.show field_ty)
      (Symbol.name actual_sym) (Type.show actual_ty)

let check_arg i param_ty (pos, arg_ty) =
  let param_ty = Type.actual_type param_ty in
  if not (Type.is_compatible param_ty arg_ty) then
    Errors.report pos "expected %s type for argument %d, got %s type"
      (Type.show param_ty) (i + 1) (Type.show arg_ty)

let rec transExpr ?(in_loop=false) (env : Env.t) (expr : expr) : Type.t =
  let rec trexpr ((pos, expr) : expr) : Type.t =
    match expr with
    | IntExpr _ -> Type.IntType
    | StrExpr _ -> Type.StringType
    | NilExpr -> Type.NilType
    | UnaryExpr { op; rand } -> trunary pos op rand
    | BinaryExpr { lhs; op; rhs } -> trbinary pos lhs rhs op
    | VarExpr var -> trvar var
    | SeqExpr exprs -> List.map ~f:trexpr exprs |> List.last_exn
    | AssignExpr { var; expr } -> trassign pos var expr
    | RecordExpr { ty; fields } -> trrecord pos ty fields
    | ArrayExpr { ty; size; init } -> trarray pos ty size init
    | CallExpr { func; args } -> trcall pos func args
    | IfExpr { cond; conseq; alt } -> trif pos cond conseq alt
    | WhileExpr { cond; body } -> trwhile pos cond body
    | ForExpr { var; low; high; body; _ } -> trfor pos var low high body
    | BreakExpr -> trbreak pos
    | LetExpr { decls; body } -> trlet pos decls body

  and trvar = function
    | SimpleVar (pos, sym) ->
      begin
        match Env.find_var env sym with
        | None ->
          report_undef_var pos sym;
          Type.IntType
        | Some ty -> Type.actual_type ty
      end
    | FieldVar (pos, var, sym) ->
      begin
        match trvar var with
        | RecordType (fields, _) as ty ->
          begin
            match List.Assoc.find fields sym ~equal:(=) with
            | None -> 
              Errors.report pos "%s type has no field named %s"
                (Type.show ty) (Symbol.name sym);
              Type.IntType
            | Some ty -> Type.actual_type ty
          end
        | ty ->
          Errors.report pos "expected record type, got %s type" (Type.show ty);
          ty
      end
    | IndexVar (pos, var, ((ix_pos, _) as expr)) ->
      begin
        check_int ix_pos (trexpr expr);
        match trvar var with
        | ArrayType (elem_ty, _) -> Type.actual_type elem_ty
        | ty ->
          Errors.report pos "expected array type, got %s type" (Type.show ty);
          ty
      end

  and trunary _pos _op ((rand_pos, _) as rand) =
    let rand_ty = trexpr rand in
    check_int rand_pos rand_ty;
    Type.IntType

  and trbinary pos ((lhs_pos, _) as lhs) ((rhs_pos, _) as rhs) op =
    let lhs_ty = trexpr lhs in
    let rhs_ty = trexpr rhs in
    match op with
    | AddOp | SubOp | MulOp | DivOp | AndOp | OrOp ->
      begin
        check_int lhs_pos lhs_ty;
        check_int rhs_pos rhs_ty;
        Type.IntType
      end
    | EqOp | NeqOp ->
      begin
        check_eq_type lhs_pos lhs_ty;
        check_eq_type rhs_pos rhs_ty;

        if not (Type.is_compatible lhs_ty rhs_ty) then
          Errors.report pos "operand types mismatch for equality operator";

        if lhs_ty = Type.NilType && rhs_ty = Type.NilType then
          Errors.report pos "cannot determine the type of the operands";

        Type.IntType
      end
    | GtOp | LtOp | GeOp | LeOp ->
      begin
        if lhs_ty <> Type.IntType && lhs_ty <> Type.StringType then
          Errors.report lhs_pos "expected int or string type, got %s type"
            (Type.show lhs_ty);

        if rhs_ty <> Type.IntType && rhs_ty <> Type.StringType then
          Errors.report rhs_pos "expected int or string type, got %s type"
            (Type.show rhs_ty);

        if lhs_ty <> rhs_ty then
          Errors.report pos "operand type mismatch for comparison operator";

        Type.IntType
      end

  and trassign pos var expr =
      let var_ty = trvar var in
      let expr_ty = trexpr expr in
      begin
        if not (Type.is_compatible var_ty expr_ty) then
          Errors.report pos "expected %s type for assignment, got %s type"
            (Type.show var_ty) (Type.show expr_ty);
        Type.UnitType
      end

  and trrecord pos ty_sym fields =
    let actual_fields = List.map fields
      ~f:(fun (pos, sym, expr) -> (pos, sym, trexpr expr))
    in
    begin
      match Env.find_type env ty_sym with
      | None ->
        Errors.report pos "undefined record type: %s" (Symbol.name ty_sym);
        Type.RecordType
          (List.map ~f:(fun (_, sym, ty) -> (sym, ty)) actual_fields,
           Type.new_unique ())
      | Some ty ->
        begin
          let ty = Type.actual_type ty in
          match ty with
          | RecordType (fields, _) ->
            if List.length fields <> List.length actual_fields then
              Errors.report pos "expected %d fields, got %d fields"
                (List.length fields) (List.length actual_fields);
            iter2 check_field fields actual_fields;
            ty
          | _ ->
            Errors.report pos "expected record type for record creation, got %s type"
              (Type.show ty);
            Type.NilType
        end
    end

  and trcall pos func args =
    let args' = List.map args ~f:(fun ((pos, _) as expr) -> (pos, trexpr expr)) in
    match Env.find_fun env func with
    | None ->
      Errors.report pos "undefined function: %s" (Symbol.name func);
      Type.IntType
    | Some (params, ret) ->
      begin
        if List.length params <> List.length args' then
          Errors.report pos "expected %d parameters, got %d arguments"
            (List.length params) (List.length args');
        iter2i check_arg params args';
        Type.actual_type ret
      end

  and trarray pos ty_sym ((size_pos, _) as size_expr) init_expr =
    let size_ty = trexpr size_expr in
    let init_ty = trexpr init_expr in
    begin
      if size_ty <> Type.IntType then
        Errors.report size_pos "expected %s type for array size, got %s type"
          (Type.show Type.IntType) (Type.show size_ty);

      match Env.find_type env ty_sym with
      | None ->
        Errors.report pos "undefined array type: %s" (Symbol.name ty_sym);
        Type.ArrayType (init_ty, Type.new_unique ())
      | Some ty ->
        let ty = Type.actual_type ty in
        match ty with
        | ArrayType (elem_ty, _) ->
          begin
            if elem_ty <> init_ty then
              Errors.report pos "expected %s type for initial value of array, got %s type"
                (Type.show elem_ty) (Type.show init_ty);
            ty
          end
        | _ ->
          Errors.report pos "expected array type for array creation, got %s type"
            (Type.show ty);
          Type.ArrayType (init_ty, Type.new_unique ())
    end

  and trif pos cond conseq alt =
    let cond_ty = trexpr cond in
    let conseq_ty = trexpr conseq in
    let alt_ty = Option.value_map alt ~default:Type.UnitType ~f:trexpr
    in
      if cond_ty <> Type.IntType then
        Errors.report pos "expected int type for condition of if expression, got %s type"
          (Type.show cond_ty);
      if not (Type.is_compatible conseq_ty alt_ty) then
        Errors.report pos "branch types mismatch for if expression";
      conseq_ty

  and trwhile pos cond body =
    let cond_ty = trexpr cond in
    let body_ty = transExpr ~in_loop:true env body in
    if cond_ty <> Type.IntType then
      Errors.report pos "expected int type for condition of while expression, got %s type"
        (Type.show cond_ty);
    if body_ty <> Type.UnitType then
      Errors.report pos "expected unit type for body of while expression, got %s type"
        (Type.show body_ty);
    Type.UnitType

  and trfor pos var low high body =
    let low_ty = trexpr low in
    let high_ty = trexpr high in

    if low_ty <> Type.IntType then
      Errors.report pos "expected int type for lower bound of for expression, got %s type"
        (Type.show low_ty);

    if high_ty <> Type.IntType then
      Errors.report pos "expected int type for higher bound of for expression, got %s type"
        (Type.show high_ty);

    let body_ty = transExpr ~in_loop:true (Env.extend_var env var Type.IntType) body in

    if body_ty <> Type.IntType then
      Errors.report pos "expected unit type for body of for expression, got %s type"
        (Type.show body_ty);

    Type.UnitType

  and trbreak pos =
    if not in_loop then
      Errors.report pos "break expression must be inside loop";
    Type.UnitType

  and trlet pos decls body = Type.UnitType

  in
    trexpr expr