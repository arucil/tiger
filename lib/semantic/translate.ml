open Base

module type S = sig
  type level
  type access

  type ir

  val outermost : level

  val new_level : level -> Temp.label -> bool list -> Temp.temp_store -> level

  val params : level -> access list

  val new_local : level -> bool -> Temp.temp_store -> access
end

(* TODO: Exercise 6.5 eliminate unnecessary static links *)

module Make (Frame : Frame.S) = struct

  type level =
    {
      parent : level option;
      frame : Frame.t;
    }

  and access = level * Frame.access

  type ir =
    | Expr of Ir.expr
    | Stmt of Ir.stmt
    | Cond of (Temp.label -> Temp.label -> Ir.stmt)

  let outermost =
    let temp_store = Temp.new_store () in
    {
      parent = None;
      frame = Frame.new_frame (Temp.named_label "main") [] temp_store
    }

  let new_level parent label params temp_store =
    {
      parent = Some parent;
      frame = Frame.new_frame label (true :: params) temp_store;
    }

  let params level =
    Frame.params level.frame
      |> List.tl_exn
      |> List.map ~f:(fun acc -> (level, acc))

  let new_local level escape temp_store =
    (level, Frame.new_local level.frame escape temp_store)


  let to_expr (ir : ir) (temp_store : Temp.temp_store) : Ir.expr =
    match ir with
    | Expr e -> e
    | Stmt s -> Ir.Eseq (s, Ir.Const 0)
    | Cond c ->
      let r = Temp.new_temp temp_store in
      let t = Temp.new_label temp_store in
      let f = Temp.new_label temp_store in
      let z = Temp.new_label temp_store in
      let open Ir in
      Eseq (
        seq [
          c t f;
          Label t;
          Move (Temp r, Const 1);
          Jump (Name z, [z]);
          Label f;
          Move (Temp r, Const 0);
          Label z;
        ],
        Temp r)

  let to_stmt (ir : ir) (temp_store : Temp.temp_store) : Ir.stmt =
    match ir with
    | Expr e -> Ir.Expr e
    | Stmt s -> s
    | Cond c ->
      let z = Temp.new_label temp_store in
      Seq (c z z, Label z)

  let to_cond (ir : ir) (temp_store : Temp.temp_store) : Temp.label -> Temp.label -> Ir.stmt =
    match ir with
    | Expr e ->
      (match e with
      | Const n ->
        if n <> 0 then fun t f -> Jump (Name t, [t])
        else fun t f -> Jump (Name f, [f])
      | _ -> fun t f -> Cjump (Eq, e, Const 0, t, f))
    | Stmt _ -> Utils.Exn.unreachable ()
    | Cond c -> c

end