open Base
open Parse
open Poly


let rec transExpr (in_loop : bool) (env : Env.t) (expr : Ast.expr) : Type.t =
  let rec trexpr ((pos, expr) : Ast.expr) : Type.t =
    let open Ast in
    match expr with
    | IntExpr _ -> Type.IntType
    | StrExpr _ -> Type.StringType
    | NilExpr -> Type.NilType
    | UnaryExpr { op; rand } -> trunary pos op rand
    | BinaryExpr { lhs; op; rhs } -> trbinary pos lhs rhs op
    | VarExpr var -> trvar var
    | SeqExpr exprs ->
      if List.is_empty exprs then
        Type.UnitType
      else
        List.map ~f:trexpr exprs |> List.last_exn
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

  (* report non-int type *)
  and check_int pos ty =
    if ty <> Type.IntType then
      Errors.report pos "expected %s type, got %s type"
        (Type.show Type.IntType) (Type.show ty)

  (* check operand type for equality/non-equality operator*)
  and check_eq_type pos ty =
    if ty <> Type.IntType && ty <> Type.StringType && ty <> Type.NilType
        && not (Type.is_record ty) && not (Type.is_array ty) then
      Errors.report pos "expected int, string, nil, record or array type, got %s type"
        (Type.show ty)

  (* report undefined variable *)
  and report_undef_var pos sym =
    Errors.report pos "undefined variable: %s" (Symbol.name sym)

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
          Errors.report pos "operand types incompatible for equality operator";

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
          Errors.report pos "operand type incompatible for comparison operator";

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
            Utils.List.iter2 check_field fields actual_fields;
            ty
          | _ ->
            Errors.report pos "expected record type for record creation, got %s type"
              (Type.show ty);
            (* empty record type *)
            Type.RecordType ([], Type.new_unique ())
        end
    end

  (* check the types of a field of a record type and that of the record creation
     are compatible *)
  and check_field (field_sym, field_ty) (pos, actual_sym, actual_ty) =
    let field_ty = Type.actual_type field_ty in
    if field_sym <> actual_sym || not (Type.is_compatible field_ty actual_ty) then
      Errors.report pos "expected field '%s' of %s type, got field '%s' of %s type"
        (Symbol.name field_sym) (Type.show field_ty)
        (Symbol.name actual_sym) (Type.show actual_ty)

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
        Utils.List.iter2i check_arg params args';
        Type.actual_type ret
      end

  (* check the types of a parameter and an argument are compatible *)
  and check_arg i param_ty (pos, arg_ty) =
    let param_ty = Type.actual_type param_ty in
    if not (Type.is_compatible param_ty arg_ty) then
      Errors.report pos "expected %s type for argument %d, got %s type"
        (Type.show param_ty) (i + 1) (Type.show arg_ty)


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
        Errors.report pos "branch types incompatible for if expression";
      conseq_ty

  and trwhile pos cond body =
    let cond_ty = trexpr cond in
    let body_ty = transExpr true env body in
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

    let body_ty = transExpr true (Env.extend_var env var Type.IntType) body in

    if body_ty <> Type.IntType then
      Errors.report pos "expected unit type for body of for expression, got %s type"
        (Type.show body_ty);

    Type.UnitType

  and trbreak pos =
    if not in_loop then
      Errors.report pos "break expression must be inside loop";
    Type.UnitType

  and trlet _pos decls body =
    transExpr in_loop (trdecls env decls) body

  and trdecls env decls =
    List.fold decls ~init:env ~f:trdecl

  and trdecl env = function
    | VarDecl { name; ty; init; pos; _ } -> trvardecl env name ty init pos
    | TypeDecl tydecls -> trtydecls env tydecls
    | FunDecl fundecls -> trfundecls env fundecls

  and trvardecl env name ty init pos =
    let init_ty = transExpr in_loop env init in
    let var_ty =
      match ty with
      | Some (ty_pos, ty_sym) ->
        begin
          match Env.find_type env ty_sym with
          | None ->
            Errors.report ty_pos "undefined type: %s" (Symbol.name ty_sym);
            if init_ty = Type.NilType then
              Type.RecordType ([], Type.new_unique ())
            else
              init_ty
          | Some ty ->
            let ty = Type.actual_type ty in
            if not (Type.is_compatible ty init_ty) then
              Errors.report pos "type incompatible for variable declaration";
            ty
        end
      | None ->
        if init_ty = Type.NilType then
          begin
            Errors.report pos "variable declaration must specify type for nil initial value";
            Type.RecordType ([], Type.new_unique ())
          end
        else
          init_ty
    in
    Env.extend_var env name var_ty

  and trtydecls env tydecls =
    let ty_names = List.map tydecls ~f:(fun d -> d.name) in
    let tys : Type.t list = List.map tydecls
      ~f:(fun { name; _ } -> Type.AliasType { name; ty = None })
    in
    (* if type decl duplicates, this will keep the latest *)
    let env' = List.fold2_exn ty_names tys ~init:env ~f:Env.extend_type in

    (* update alias types *)
    List.iter2_exn tydecls tys
      ~f:(fun { ty; _ } name_ty ->
            match name_ty with
            | AliasType r ->
              r.ty <- Some (trty env' ty)
            | _ ->
              Utils.Exn.unreachable ());

    (* report duplicate type declarations *)
    ignore (List.fold tydecls ~init:[]
      ~f:(fun tydecls tydecl ->
            if List.exists tydecls ~f:(tydecl_name_eq tydecl) then
              Errors.report tydecl.pos "duplicate type declaration: %s"
                (Symbol.name tydecl.name);
            tydecl :: tydecls));

    (* report cyclic type declarations *)
    let rec check_cycle ty_pos accum_ty_names ty_name =
      match Env.find_type env ty_name with
      | None -> Utils.Exn.unreachable ()
      | Some (Type.AliasType { ty; _ }) ->
        (match ty with
        | None -> Utils.Exn.unreachable ()
        | Some (Type.AliasType alias) ->
          if List.mem accum_ty_names alias.name ~equal:Poly.(=) then
            begin
              let cyclic_names = (alias.name :: List.take_while accum_ty_names
                ~f:(Poly.(<>) alias.name))
              in
              Errors.report ty_pos "cyclic type detected: %s"
                (List.map cyclic_names ~f:Symbol.name |> String.concat ~sep:", ");
              cyclic_names
            end
          else
            check_cycle ty_pos (alias.name :: accum_ty_names) alias.name
        | Some _ -> [])
      | Some _ -> []
    in
    (* fold from right so that the earlier duplicate type declarations won't be checked *)
    ignore (List.fold_right tydecls ~init:[]
      ~f:(fun tydecl accum_ty_names ->
            (* if type duplicates or cycles are detected, these types won't be checked again *)
            if List.mem accum_ty_names tydecl.name ~equal:Poly.(=) then
              accum_ty_names
            else
              let cyclic_names = check_cycle tydecl.pos [tydecl.name] tydecl.name in
              tydecl.name :: (cyclic_names @ accum_ty_names)));

    env'

  and tydecl_name_eq (decl1 : Ast.typedecl) (decl2 : Ast.typedecl) =
    decl1.name = decl2.name

  and trty env : Ast.ty -> Type.t = function
    | AliasType (pos, sym) -> sym_type env pos sym
    | ArrayType (pos, sym) -> Type.ArrayType (sym_type env pos sym, Type.new_unique ())
    | RecordType fields ->
      let fields' = List.map fields
        ~f:(fun { name; ty; pos } -> (name, sym_type env pos ty))
      in

      (* report duplicate fields *)
      ignore (List.fold fields ~init:[]
        ~f:(fun fields field ->
              if List.exists fields ~f:(field_name_eq field) then
                Errors.report field.pos "duplicate field: %s"
                  (Symbol.name field.name);
              field :: fields));

      (* if fields have duplicates, keep the latest *)
      let fields' = List.fold_right fields' ~init:[]
        ~f:(fun ((sym, _) as field) fields ->
              if List.Assoc.mem fields ~equal:(=) sym then
                fields
              else
                field :: fields)
      in
        Type.RecordType (fields', Type.new_unique ())

  and field_name_eq (field1 : Ast.field) (field2 : Ast.field) =
    field1.name = field2.name

  (* perform semantic analysis on consecutive function declarations *)
  and trfundecls env fundecls =
    let fun_tys = List.map fundecls ~f:(fun d ->
      let param_tys = List.map d.params
        ~f:(fun p -> Type.actual_type (sym_type env p.pos p.ty))
      in
      let ret_ty =
        match d.ret with
        | None -> Type.UnitType
        | Some (ret_pos, ret_sym) ->
          Type.actual_type (sym_type env ret_pos ret_sym)
      in
        (param_tys, ret_ty))
    in
    let fun_names = List.map fundecls ~f:(fun d -> d.name) in
    let env' = List.fold2_exn fun_names fun_tys ~init:env ~f:Env.extend_fun in

    (* check each function *)
    List.iter2_exn fundecls fun_tys ~f:(fun fundecl (param_tys, ret_ty) ->
      check_dup_params [] fundecl.params;

      let param_names = List.map fundecl.params ~f:(fun p -> p.name) in
      let body_ty = transExpr false
        (List.fold2_exn param_names param_tys ~init:env' ~f:Env.extend_var)
        fundecl.body
      in
      if not (Type.is_compatible ret_ty body_ty) then
        Errors.report fundecl.pos "expected %s type for function body, got %s type"
          (Type.show ret_ty) (Type.show body_ty));

    env'

  and check_dup_params accum_param_names = function
    | [] -> ()
    | param :: params ->
      if List.mem accum_param_names param.name ~equal:Poly.(=) then
        (Errors.report param.pos "duplicate paramater: %s" (Symbol.name param.name);
        check_dup_params accum_param_names params)
      else
        check_dup_params (param.name :: accum_param_names) params

  and sym_type env pos sym =
    match Env.find_type env sym with
    | None ->
      Errors.report pos "undefined type: %s" (Symbol.name sym);
      Type.IntType
    | Some ty -> ty

  in
    trexpr expr

let transProg expr = transExpr false Env.predefined expr