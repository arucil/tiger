open Base
open Semantic
open Ir

let linearize stmt temp_store =
  let (++) s1 s2 =
    match s1, s2 with
    | Expr (Const _ | Name _), _ -> s2
    | _, Expr (Const _ | Name _) -> s1
    | _ -> Seq (s1, s2)
  in

  (* TODO: more powerful commutivity test *)
  let commute s e =
    match s, e with
    | Expr (Const _ | Name _), _ -> true
    | _, (Const _ | Name _) -> true
    | _ -> false
  in

  let rec reorder = function
    | [] -> (Expr (Const 0), [])
    | ((Call _) as e) :: exprs ->
      let t = Temp.new_temp temp_store in
      reorder (Eseq (Move (Temp t, e), Lval (Temp t)) :: exprs)
    | expr :: exprs ->
      let (s1, expr) = do_expr expr in
      let (s2, exprs) = reorder exprs in
      if commute s2 expr then
        (s1 ++ s2, expr :: exprs)
      else
        let t = Temp.new_temp temp_store in
        (s1 ++ Move (Temp t, expr) ++ s2, Lval (Temp t) :: exprs)

  and reorder_stmt exprs f =
    let (stmt, exprs) = reorder exprs
    in stmt ++ f exprs

  and reorder_expr exprs f =
    let (stmt, exprs) = reorder exprs
    in (stmt, f exprs)

  and do_stmt = function
    | Seq (s1, s2) ->
      do_stmt s1 ++ do_stmt s2
    | Jump (e, labels) ->
      reorder_stmt [e] (function
        | [e] -> Jump (e, labels)
        | _ -> Utils.Exn.unreachable ())
    | Cjump (op, e1, e2, t, f) ->
      reorder_stmt [e1; e2] (function
        | [e1; e2] -> Cjump (op, e1, e2, t, f)
        | _ -> Utils.Exn.unreachable ())
    | Move (Temp t, Call (f, args)) ->
      reorder_stmt (f :: args) (function
        | (f :: args) -> Move (Temp t, Call (f, args))
        | _ -> Utils.Exn.unreachable ())
    | Move (Temp t, e) ->
      reorder_stmt [e] (function
        | [e] -> Move (Temp t, e)
        | _ -> Utils.Exn.unreachable ())
    | Move (Mem m, e) ->
      reorder_stmt [m; e] (function
        | [m; e] -> Move (Mem m, e)
        | _ -> Utils.Exn.unreachable ())
    | Expr (Call (f, args)) ->
      reorder_stmt (f :: args) (function
        | (f :: args) -> Expr (Call (f, args))
        | _ -> Utils.Exn.unreachable ())
    | Expr e ->
      reorder_stmt [e] (function
        | [e] -> Expr e
        | _ -> Utils.Exn.unreachable ())
    | s -> s

  and do_expr = function
    | Binop (op, e1, e2) ->
      reorder_expr [e1; e2] (function
        | [e1; e2] -> Binop (op, e1, e2)
        | _ -> Utils.Exn.unreachable ())
    | Unop (op, e) ->
      reorder_expr [e] (function
        | [e] -> Unop (op, e)
        | _ -> Utils.Exn.unreachable ())
    | Call _ ->
      Utils.Exn.unreachable ()
    | Eseq (s, e) ->
      let s = do_stmt s in
      let (s1, e) = do_expr e in
      (s ++ s1, e)
    | Lval (Mem e) ->
      reorder_expr [e] (function
        | [e] -> Lval (Mem e)
        | _ -> Utils.Exn.unreachable ())
    | e -> (Expr (Const 0), e)

  in

  let rec flatten stmt stmts =
    match stmt with
    | Ir.Seq (l, r) -> flatten l (flatten r stmts)
    | _ -> stmt :: stmts
  in
    flatten (do_stmt stmt) []


let basic_blocks stmts temp_store =
  let done_label = Temp.new_label temp_store in
  let rec build_blocks stmts blocks =
    let rec build_block stmts block =
      match stmts with
      | [] -> build_block [Jump (Name done_label, [done_label])] block
      | (Jump _ | Cjump _ as stmt) :: stmts ->
        build_blocks stmts (List.rev (stmt :: block) :: blocks)
      | Label label :: _ ->
        build_block (Jump (Name label, [label]) :: stmts) block
      | stmt :: stmts' ->
        build_block stmts' (stmt :: block)
    in
    match stmts with
    | [] -> List.rev blocks
    | Label _ as stmt :: stmts' ->
      build_block stmts' [stmt]
    | _ :: _ ->
      let label = Temp.new_label temp_store in
      build_block stmts [Label label]
  in
    (build_blocks stmts [], done_label)

let trace_schedule blocks done_label temp_store =
  let rec gather_traces table = function
    | (Label label :: _) as block :: blocks ->
      (match Symtab.find table label with
       | None -> gather_traces table blocks
       | Some _ -> gather_trace table block blocks)
    | [] -> []
    | _ -> Utils.Exn.unreachable ()

  and gather_trace table block blocks =
    let table =
      Symtab.remove table
        (match block with
         | Label l :: _ -> l
         | _ -> Utils.Exn.unreachable ())
    in
    match List.last_exn block with
    | Jump (Name l, _) ->
      begin
        match Symtab.find table l with
        | None ->
          block @ gather_traces table blocks
        | Some ((Label l :: _) as b) ->
          Utils.List.init_exn block
            @ gather_trace table b blocks
        | _ ->
          Utils.Exn.unreachable ()
      end
    | Cjump (op, l, r, t, f) ->
      begin
        match Symtab.find table t, Symtab.find table f with
        | _, Some b ->
          block @ gather_trace table b blocks
        | Some b, _ ->
          Utils.List.init_exn block
            @ [Cjump (op, r, l, f, t)]
            @ gather_trace table b blocks
        | None, None ->
          let t = Temp.new_label temp_store in
          f
      end
    | _ ->
      Utils.Exn.unreachable ()

  in
  let newentry table block =
    match block with
    | Label label :: _ -> Symtab.extend table label block
    | _ -> Utils.Exn.unreachable ()

  in
    gather_traces (List.fold blocks ~init:Symtab.empty ~f:newentry) blocks @ [Label done_label]