open Base
open Common

let trans_prog' (expr : Ast.expr) (module Trans : Translate.S) temp_store =
  let unique_store = Type.new_unique_store () in
  let new_unique () = Type.new_unique unique_store in
  let module Env = Env.Make (Trans) in

  Trans.set_temp_store temp_store;

  let rec trans_expr (level : Trans.level) (break : Temp.label option) (env : Env.t) (expr : Ast.expr) : Type.t * Translate.ir =
    let rec trexpr ((pos, expr) : Ast.expr) : Type.t * Translate.ir =
      let open Ast in
      match expr with
      | IntExpr n -> (Type.IntType, Trans.int n)
      | StrExpr s -> (Type.StringType, Trans.str s)
      | NilExpr -> (Type.NilType, Trans.nil)
      | UnaryExpr { op; rand } -> trunary pos op rand
      | BinaryExpr { lhs; op; rhs } -> trbinary pos lhs rhs op
      | VarExpr var -> fst (trvar var)
      | SeqExpr exprs ->
        if List.is_empty exprs then
          (Type.UnitType, Trans.unit)
        else
          let (tys, irs) = List.unzip (List.map exprs ~f:trexpr) in
          (List.last_exn tys, Trans.seq irs)
      | AssignExpr { var; expr } -> trassign pos var expr
      | RecordExpr { ty; fields } -> trrecord pos ty fields
      | ArrayExpr { ty; size; init } -> trarray pos ty size init
      | CallExpr { func; args } -> trcall pos func args
      | IfExpr { cond; conseq; alt } -> trif pos cond conseq alt
      | WhileExpr { cond; body } -> trwhile pos cond body
      | ForExpr { var; low; high; body; escape } -> trfor pos var !escape low high body
      | BreakExpr -> trbreak pos
      | LetExpr { decls; body } -> trlet pos decls body

    and trvar = function
      | SimpleVar (pos, sym) ->
        begin
          match Env.find_var env sym with
          | None ->
            Errors.report pos "undefined variable: %s" (Symbol.name sym);
            ((Type.IntType, Trans.error), true)
          | Some (FunEntry _) ->
            Errors.report pos "cannot use function as variable: %s" (Symbol.name sym);
            ((Type.IntType, Trans.error), true)
          | Some (VarEntry { ty; assignable; access }) ->
            let ir = Trans.simple_var level access in
            ((Type.actual_type ty, ir), assignable)
        end
      | FieldVar (pos, var, sym) ->
        begin
          match trvar var with
          | (RecordType (fields, _) as ty, rec_ir), _ ->
            begin
              match List.Assoc.find fields sym ~equal:Symbol.(=) with
              | None -> 
                Errors.report pos "%s type has no field named %s"
                  (Type.show ty) (Symbol.name sym);
                ((Type.IntType, Trans.error), true)
              | Some ty ->
                let ir = Trans.field_var ~record:rec_ir sym fields in
                ((Type.actual_type ty, ir), true)
            end
          | (ty, _), _ ->
            Errors.report pos "expected record type for field selection, got %s type" (Type.show ty);
            ((ty, Trans.error), true)
        end
      | IndexVar (pos, var, ((ix_pos, _) as expr)) ->
        begin
          let (ix_ty, ix_ir) = trexpr expr in
          check_int ix_pos ix_ty "index";
          match trvar var with
          | (ArrayType (elem_ty, _), arr_ir), _ ->
            let ir = Trans.index_var ~array:arr_ir ~index:ix_ir in
            ((Type.actual_type elem_ty, ir), true)
          | (ty, _), _ ->
            Errors.report pos "cannot index on %s type" (Type.show ty);
            ((ty, Trans.error), true)
        end

    (* report non-int type, if fail return true *)
    and check_int pos ty operand =
      if Type.(ty <> IntType) then
        Errors.report pos "expected int type for %s, got %s type"
          operand (Type.show ty)

    (* check operand type for equality/non-equality operator*)
    and check_eq_type pos ty operand =
      if Type.(ty <> IntType && ty <> StringType && ty <> NilType)
          && not (Type.is_record ty) && not (Type.is_array ty) then
        Errors.report pos "expected int, string, record or array type for %s, got %s type"
          operand (Type.show ty)

    and trunary _pos op ((rand_pos, _) as rand) =
      let (rand_ty, rand_ir) = trexpr rand in
      check_int rand_pos rand_ty "operand of negation";
      let ir = Trans.unary op rand_ir in
      (Type.IntType, ir)

    and trbinary pos ((lhs_pos, _) as lhs) ((rhs_pos, _) as rhs) op =
      let (lhs_ty, lhs_ir) = trexpr lhs in
      let (rhs_ty, rhs_ir) = trexpr rhs in
      let ty =
        (match op with
        | AddOp | SubOp | MulOp | DivOp | AndOp | OrOp ->
          begin
            check_int lhs_pos lhs_ty ("left-hand side of " ^ opstr op);
            check_int rhs_pos rhs_ty ("right-hand side of " ^ opstr op);
            Type.IntType
          end
        | EqOp | NeqOp ->
          begin
            check_eq_type lhs_pos lhs_ty ("left-hand side of " ^ opstr op);
            check_eq_type rhs_pos rhs_ty ("right-hand side of " ^ opstr op);

            if not (Type.is_compatible lhs_ty rhs_ty) then
              Errors.report pos "incompatible operand types for %s"
                (opstr op);

            if Type.(lhs_ty = NilType && rhs_ty = NilType) then
              Errors.report pos "cannot determine the types of the operands";

            Type.IntType
          end
        | GtOp | LtOp | GeOp | LeOp ->
          begin
            if Type.(lhs_ty <> IntType && lhs_ty <> StringType) then
              Errors.report lhs_pos "expected int or string type for left-hand side of %s, got %s type"
                (opstr op) (Type.show lhs_ty);

            if Type.(rhs_ty <> IntType && rhs_ty <> StringType) then
              Errors.report rhs_pos "expected int or string type for right-hand side of %s, got %s type"
                (opstr op) (Type.show rhs_ty);

            if Type.(lhs_ty <> rhs_ty) then
              Errors.report pos "incompatible operand types for %s" (opstr op);

            Type.IntType
          end)
        in
        let ir =
          if Type.(lhs_ty = Type.StringType && rhs_ty = Type.StringType) then
            Trans.str_binary op lhs_ir rhs_ir
          else
            Trans.binary op lhs_ir rhs_ir
        in
          (ty, ir)

    and opstr : Ast.op -> string = function
      | AddOp -> "'+'"
      | SubOp -> "'-'"
      | MulOp -> "'*'"
      | DivOp -> "'/'"
      | AndOp -> "'&'"
      | OrOp -> "'|'"
      | EqOp -> "'='"
      | NeqOp -> "'<>'"
      | GtOp -> "'>'"
      | GeOp -> "'>='"
      | LtOp -> "'<'"
      | LeOp -> "'<='"

    and trassign _pos var ((expr_pos, _) as expr) =
        let ((var_ty, var_ir), assignable) = trvar var in
        let (expr_ty, expr_ir) = trexpr expr in
        begin
          if not assignable then
            (match var with
            | SimpleVar (pos, sym) ->
              Errors.report pos "variable cannot be assigned to: %s"
                (Symbol.name sym)
            | _ -> Utils.Exn.unreachable ());
          if not (Type.is_compatible var_ty expr_ty) then
            Errors.report expr_pos "expected %s type for right-hand side of assignment, got %s type"
              (Type.show var_ty) (Type.show expr_ty);
          let ir = Trans.assign ~var:var_ir ~expr:expr_ir in
          (Type.UnitType, ir)
        end

    and trrecord pos ty_sym fields =
      let (actual_fields, field_irs) =
        List.unzip (List.map fields
          ~f:(fun (pos, sym, expr) ->
            let (ty, ir) = trexpr expr in
            ((pos, sym, ty), ir)))
      in
      begin
        match Env.find_type env ty_sym with
        | None ->
          Errors.report pos "undefined record type: %s" (Symbol.name ty_sym);
          (Type.RecordType
            (List.map ~f:(fun (_, sym, ty) -> (sym, ty)) actual_fields,
            new_unique ()),
           Trans.error)
        | Some ty ->
          begin
            let ty = Type.actual_type ty in
            match ty with
            | RecordType (fields, _) ->
              if List.length fields <> List.length actual_fields then
                Errors.report pos "expected %d fields, got %d fields"
                  (List.length fields) (List.length actual_fields);
              Utils.List.iter2 ~f:check_field fields actual_fields;
              let ir = Trans.record field_irs in
              (ty, ir)
            | _ ->
              Errors.report pos "expected record type for record creation, got %s type"
                (Type.show ty);
              (* empty record type *)
              (Type.RecordType ([], new_unique ()), Trans.error)
          end
      end

    (* check the types of a field of a record type and that of the record creation
      are compatible *)
    and check_field (field_sym, field_ty) (pos, actual_sym, actual_ty) =
      let field_ty = Type.actual_type field_ty in
      if Symbol.(field_sym <> actual_sym) || not (Type.is_compatible field_ty actual_ty) then
        Errors.report pos "expected field '%s' of %s type, got field '%s' of %s type"
          (Symbol.name field_sym) (Type.show field_ty)
          (Symbol.name actual_sym) (Type.show actual_ty)

    and trcall pos func args =
      let (args', arg_irs) =
        List.unzip (List.map args ~f:(fun ((pos, _) as expr) ->
          let (ty, ir) = trexpr expr in
          ((pos, ty), ir)))
      in
      match Env.find_var env func with
      | None ->
        Errors.report pos "undefined function: %s" (Symbol.name func);
        (Type.IntType, Trans.error)
      | Some (VarEntry _) ->
        Errors.report pos "cannot call non-function variable: %s" (Symbol.name func);
        (Type.IntType, Trans.error)
      | Some (FunEntry { params; ret; level = fun_level; label }) ->
        begin
          if List.length params <> List.length args' then
            Errors.report pos "expected %d parameters, got %d arguments"
              (List.length params) (List.length args');
          Utils.List.iter2i ~f:check_arg params args';
          let ir = Trans.call ~fun_level ~use_level:level label arg_irs in
          (Type.actual_type ret, ir)
        end

    (* check the types of a parameter and an argument are compatible *)
    and check_arg i param_ty (pos, arg_ty) =
      let param_ty = Type.actual_type param_ty in
      if not (Type.is_compatible param_ty arg_ty) then
        Errors.report pos "expected %s type for argument %d, got %s type"
          (Type.show param_ty) (i + 1) (Type.show arg_ty)


    and trarray pos ty_sym ((size_pos, _) as size_expr) init_expr =
      let (size_ty, size_ir) = trexpr size_expr in
      let (init_ty, init_ir) = trexpr init_expr in
      begin
        if Type.(size_ty <> IntType) then
          Errors.report size_pos "expected int type for array size, got %s type"
            (Type.show size_ty);

        match Env.find_type env ty_sym with
        | None ->
          Errors.report pos "undefined array type: %s" (Symbol.name ty_sym);
          (Type.ArrayType (init_ty, new_unique ()), Trans.error)
        | Some ty ->
          let ty = Type.actual_type ty in
          match ty with
          | ArrayType (elem_ty, _) ->
            begin
              let elem_ty = Type.actual_type elem_ty in
              if Type.(elem_ty <> init_ty) then
                Errors.report pos "expected %s type for initial value of array, got %s type"
                  (Type.show elem_ty) (Type.show init_ty);
              let ir = Trans.array ~size:size_ir ~init:init_ir in
              (ty, ir)
            end
          | _ ->
            Errors.report pos "expected array type for array creation, got %s type"
              (Type.show ty);
            (Type.ArrayType (init_ty, new_unique ()), Trans.error)
      end

    and trif pos cond conseq alt =
      let (cond_ty, cond_ir) = trexpr cond in
      let (conseq_ty, conseq_ir) = trexpr conseq in
      let (alt_ty, alt_ir) = Option.value_map alt ~default:(Type.UnitType, Trans.unit) ~f:trexpr
      in
      let ir =
        if Type.(conseq_ty = UnitType && alt_ty = UnitType) then
          Trans.if_stmt ~cond:cond_ir ~conseq:conseq_ir ~alt:alt_ir
        else
          Trans.if' ~cond:cond_ir ~conseq:conseq_ir ~alt:alt_ir
      in
        if Type.(cond_ty <> IntType) then
          Errors.report pos "expected int type for condition of if expression, got %s type"
            (Type.show cond_ty);
        if not (Type.is_compatible conseq_ty alt_ty) then
          Errors.report pos "incompatible branch types for if expression";
        (conseq_ty, ir)

    and trwhile pos cond body =
      let (cond_ty, cond_ir) = trexpr cond in
      if Type.(cond_ty <> IntType) then
        Errors.report pos "expected int type for condition of while loop, got %s type"
          (Type.show cond_ty);

      let break_label = Temp.new_label temp_store in
      let (body_ty, body_ir) = trans_expr level (Some break_label) env body in
      if Type.(body_ty <> UnitType) then
        Errors.report pos "expected unit type for body of while loop, got %s type"
          (Type.show body_ty);
      let ir = Trans.while' ~break:break_label ~cond:cond_ir ~body:body_ir in
      (Type.UnitType, ir)

    and trfor pos var escape low high body =
      let (low_ty, low_ir) = trexpr low in
      let (high_ty, high_ir) = trexpr high in

      check_int pos low_ty "lower bound of for loop";
      check_int pos high_ty "higher bound of for loop";

      let access = Trans.new_local level escape in

      let break_label = Temp.new_label temp_store in

      let (body_ty, body_ir) = trans_expr level (Some break_label)
        (Env.extend_var env var
          (VarEntry { access; ty = Type.IntType; assignable = false })) body
      in

      if Type.(body_ty <> UnitType) then
        Errors.report pos "expected unit type for body of for loop, got %s type"
          (Type.show body_ty);

      let ir = Trans.for' ~break:break_label access ~low:low_ir ~high:high_ir ~body:body_ir in
      (Type.UnitType, ir)

    and trbreak pos =
      match break with
      | None ->
        Errors.report pos "break expression must be inside loop";
        (Type.UnitType, Trans.error)
      | Some label ->
        (Type.UnitType, Trans.break label)

    and trlet _pos decls body =
      let (env', init_irs) = trdecls env decls in
      let (body_ty, body_ir) = trans_expr level break env' body in
      (body_ty, Trans.let' ~inits:init_irs ~body:body_ir)

    and trdecls env decls =
      List.fold decls ~init:(env, []) ~f:trdecl

    and trdecl (env, init_irs) (decl : Ast.decl) =
      match decl with
      | VarDecl { name; ty; init; pos; escape } ->
        let (env', init_ir) = trvardecl env name ty init !escape pos in
        (env', init_irs @ [init_ir])
      | TypeDecl tydecls -> (trtydecls env tydecls, init_irs)
      | FunDecl fundecls -> (trfundecls env fundecls, init_irs)

    and trvardecl env name ty init escape pos : Env.t * Translate.ir =
      let (init_ty, init_ir) = trans_expr level break env init in
      let var_ty =
        match ty with
        | Some (ty_pos, ty_sym) ->
          begin
            match Env.find_type env ty_sym with
            | None ->
              Errors.report ty_pos "undefined type: %s" (Symbol.name ty_sym);
              if Type.(init_ty = NilType) then
                Type.RecordType ([], new_unique ())
              else
                init_ty
            | Some ty ->
              let ty = Type.actual_type ty in
              if not (Type.is_compatible ty init_ty) then
                Errors.report pos "expected %s type for variable initialization, got %s type"
                  (Type.show ty) (Type.show init_ty);
              ty
          end
        | None ->
          if Type.(init_ty = NilType) then
            begin
              Errors.report pos "variable declaration must specify type for nil initial value";
              Type.RecordType ([], new_unique ())
            end
          else
            init_ty
      in
      let access = Trans.new_local level escape in
      let ir = Trans.init_var access init_ir in
      (Env.extend_var env name (VarEntry { access; ty = var_ty; assignable = true }), ir)

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
      let rec check_cycle ignored_ty_names ty_pos accum_ty_names ty_name =
        match Env.find_type env' ty_name with
        | None -> Utils.Exn.unreachable ()
        | Some (Type.AliasType { ty; _ }) ->
          (match ty with
          | None -> Utils.Exn.unreachable ()
          | Some (Type.AliasType alias) ->
            if List.mem accum_ty_names alias.name ~equal:Symbol.(=) then
              begin
                let cyclic_names = (alias.name :: List.take_while accum_ty_names
                  ~f:(Symbol.(<>) alias.name))
                in
                if not (List.exists cyclic_names
                  ~f:(fun name -> List.mem ignored_ty_names name ~equal:Symbol.(=)))
                  then
                  Errors.report ty_pos "type alias cycle detected: %s"
                    (List.map cyclic_names ~f:Symbol.name |> String.concat ~sep:", ");
                cyclic_names
              end
            else
              check_cycle ignored_ty_names ty_pos (alias.name :: accum_ty_names) alias.name
          | Some _ -> [])
        | Some _ -> []
      in
      (* fold from right so that earlier duplicate type declarations won't be checked *)
      ignore (List.fold_right tydecls ~init:[]
        ~f:(fun tydecl accum_ty_names ->
              (* if type duplicates or cycles are detected, these types won't be checked again *)
              if List.mem accum_ty_names tydecl.name ~equal:Symbol.(=) then
                accum_ty_names
              else
                let cyclic_names = check_cycle accum_ty_names tydecl.pos [tydecl.name] tydecl.name in
                tydecl.name :: (cyclic_names @ accum_ty_names)));

      env'

    and tydecl_name_eq (decl1 : Ast.typedecl) (decl2 : Ast.typedecl) =
      Symbol.(decl1.name = decl2.name)

    and trty env : Ast.ty -> Type.t = function
      | AliasType (pos, sym) -> sym_type env pos sym
      | ArrayType (pos, sym) -> Type.ArrayType (sym_type env pos sym, new_unique ())
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
                if List.Assoc.mem fields ~equal:Symbol.(=) sym then
                  fields
                else
                  field :: fields)
        in
          Type.RecordType (fields', new_unique ())

    and field_name_eq (field1 : Ast.field) (field2 : Ast.field) =
      Symbol.(field1.name = field2.name)

    (* perform semantic analysis on consecutive function declarations *)
    and trfundecls env fundecls =
      let labels = Temp.new_labels (List.length fundecls) temp_store in
      let funs = List.map2_exn labels fundecls ~f:(fun label d ->
        let param_tys = List.map d.params
          ~f:(fun p -> Type.actual_type (sym_type env p.pos p.ty))
        in
        let ret_ty =
          match d.ret with
          | None -> Type.UnitType
          | Some (ret_pos, ret_sym) ->
            Type.actual_type (sym_type env ret_pos ret_sym)
        in
        let escapes = List.map d.params ~f:(fun p -> !(p.escape)) in
        (Trans.new_level level label escapes, param_tys, ret_ty))
      in
      let fun_names = List.map fundecls ~f:(fun d -> d.name) in
      let env' = Utils.List.fold3 labels fun_names funs ~init:env
        ~f:(fun env label name (level, params, ret) -> Env.extend_var env name
          (FunEntry
            {
              label;
              level;
              params;
              ret;
            }))
      in

      (* report duplicate function declarations *)
      ignore (List.fold fundecls ~init:[]
        ~f:(fun fundecls fundecl ->
              if List.exists fundecls ~f:(fundecl_name_eq fundecl) then
                Errors.report fundecl.pos "duplicate function declaration: %s"
                  (Symbol.name fundecl.name);
              fundecl :: fundecls));

      (* check each function *)
      List.iter2_exn fundecls funs ~f:(fun fundecl (level, param_tys, ret_ty) ->
        check_dup_params [] fundecl.params;

        let param_names = List.map fundecl.params ~f:(fun p -> p.name) in
        let (body_ty, body_ir) = trans_expr level None
          (Utils.List.fold3 param_names param_tys (Trans.params level)
            ~init:env'
            ~f:(fun env name ty access ->
              Env.extend_var env name (VarEntry { access; ty; assignable = true })))
          fundecl.body
        in
        if not (Type.is_compatible ret_ty body_ty) then
          Errors.report fundecl.pos "expected %s type for function body, got %s type"
            (Type.show ret_ty) (Type.show body_ty);
        Trans.fun' level body_ir);

      env'

    and fundecl_name_eq (decl1 : Ast.fundecl) (decl2 : Ast.fundecl) =
      Symbol.(decl1.name = decl2.name)

    and check_dup_params accum_param_names = function
      | [] -> ()
      | param :: params ->
        if List.mem accum_param_names param.name ~equal:Symbol.(=) then
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

  in
  let (ty, ir) = trans_expr Trans.outermost None Env.predefined expr in
  (ty, Trans.to_stmt ir)

let trans_prog (expr : Ast.expr) (module Trans : Translate.S) temp_store : Type.t * Ir.stmt =
  Find_escape.find_escape expr;
  trans_prog' expr (module Trans) temp_store