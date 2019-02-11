open Base
open Parse

type ir =
  | Expr of Ir.expr
  | Stmt of Ir.stmt
  | Cond of (Temp.label -> Temp.label -> Ir.stmt)

module type S = sig
  type level
  type access

  val set_temp_store : Temp.temp_store -> unit

  val outermost : level

  val new_level : level -> Temp.label -> bool list -> level

  val params : level -> access list

  val new_local : level -> bool -> Temp.temp_store -> access

  val error : ir

  val simple_var : level -> access -> ir

  val field_var : record:ir -> Symbol.t -> (Symbol.t * Type.t) list -> ir

  val index_var : array:ir -> index:ir -> ir

  val int : int -> ir

  val str : string -> ir

  val nil : ir

  val unit : ir

  val unary : Ast.uop -> ir -> ir

  val binary : Ast.op -> ir -> ir -> ir

  val str_binary : Ast.op -> ir -> ir -> ir

  val if_stmt : cond:ir -> conseq:ir -> alt:ir -> ir

  val if' : cond:ir -> conseq:ir -> alt:ir -> ir

end


(* TODO: Exercise 6.5 eliminate unnecessary static links *)

module Make (Frame : Frame.S) = struct

  type level =
    {
      parent : level option;
      frame : Frame.t;
    }

  and access = level * Frame.access

  let temp_store = ref (Temp.new_store ())

  let set_temp_store store = temp_store := store

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

  let new_local level escape =
    (level, Frame.new_local level.frame escape !temp_store)


  let to_expr (ir : ir) : Ir.expr =
    match ir with
    | Expr e -> e
    | Stmt s -> Ir.Eseq (s, Ir.Const 0)
    | Cond c ->
      let r = Temp.new_temp !temp_store in
      let t = Temp.new_label !temp_store in
      let f = Temp.new_label !temp_store in
      let open Ir in
      Eseq (
        seq [
          Move (Temp r, Const 1);
          c t f;
          Label f;
          Move (Temp r, Const 0);
          Label t;
        ],
        Temp r)

  let to_stmt (ir : ir) : Ir.stmt =
    match ir with
    | Expr e -> Ir.Expr e
    | Stmt s -> s
    | Cond c ->
      let z = Temp.new_label !temp_store in
      Seq (c z z, Label z)

  let to_cond (ir : ir) : Temp.label -> Temp.label -> Ir.stmt =
    match ir with
    | Expr e ->
      (match e with
      | Const n ->
        if n <> 0 then fun t _ -> Jump (Name t, [t])
        else fun _ f -> Jump (Name f, [f])
      | _ -> fun t f -> Cjump (Eq, e, Const 0, t, f))
    | Stmt _ -> Utils.Exn.unreachable ()
    | Cond c -> c

  let error = Expr (Ir.Const 0)

  let simple_var use_level (def_level, access) =
    if Errors.has_errors () then
      error
    else
      let rec build_static_links level expr =
        if phys_equal level def_level then
          expr
        else
          (let static_link = List.hd_exn (Frame.params level.frame) in
          build_static_links (Option.value_exn level.parent)
            (Frame.access_expr static_link expr))
      in
      Expr (Frame.access_expr access (build_static_links use_level (Temp Frame.fp)))
  
  (* TODO: emit code that check nil *)
  let field_var ~record sym fields =
    if Errors.has_errors () then
      error
    else
      let i = Utils.List.find_index fields
        ~f:(fun (sym', _) -> Symbol.(=) sym sym')
      in
      let open Ir in
      Expr (Mem (Binop (Add, to_expr record, Binop (Mul, Const i, Const Frame.word_size))))

  (* TODO: emit code that check out-of-bound *)
  let index_var ~array ~index =
    if Errors.has_errors () then
      error
    else
      let open Ir in
      Expr (Mem (Binop (Add, to_expr array, Binop (Mul, to_expr index, Const Frame.word_size))))

  let int n =
    let open Ir in
    Expr (Const n)

  let str s = ()

  let nil = Expr (Const 0)

  let unit = Stmt (Ir.Expr (Ir.Const 0))

  let unary op rand =
    if Errors.has_errors () then
      error
    else
      let open Ir in
      match op with
      | Ast.NegOp -> Expr (Unop (Neg, to_expr rand))

  let binary op lhs rhs : ir =
    if Errors.has_errors () then
      error
    else
      let open Ir in
      match op with
      | Ast.AddOp -> Expr (Binop (Add, to_expr lhs, to_expr rhs))
      | SubOp -> Expr (Binop (Sub, to_expr lhs, to_expr rhs))
      | MulOp -> Expr (Binop (Mul, to_expr lhs, to_expr rhs))
      | DivOp -> Expr (Binop (Div, to_expr lhs, to_expr rhs))
      | EqOp ->
        Cond (fun t f ->
          Cjump (Eq, to_expr lhs, to_expr rhs, t, f))
      | NeqOp ->
        Cond (fun t f ->
          Cjump (Ne, to_expr lhs, to_expr rhs, t, f))
      | LtOp ->
        Cond (fun t f ->
          Cjump (Lt, to_expr lhs, to_expr rhs, t, f))
      | LeOp ->
        Cond (fun t f ->
          Cjump (Le, to_expr lhs, to_expr rhs, t, f))
      | GtOp ->
        Cond (fun t f ->
          Cjump (Gt, to_expr lhs, to_expr rhs, t, f))
      | GeOp ->
        Cond (fun t f ->
          Cjump (Ge, to_expr lhs, to_expr rhs, t, f))
      | AndOp ->
        let cl = to_cond lhs in
        let cr = to_cond rhs in
        let z = Temp.new_label !temp_store in
        Cond (fun t f ->
          Ir.seq [
            cl z f;
            Label z;
            cr t f;
          ])
      | OrOp -> 
        let cl = to_cond lhs in
        let cr = to_cond rhs in
        let z = Temp.new_label !temp_store in
        Cond (fun t f ->
          Ir.seq [
            cl t z;
            Label z;
            cr t f;
          ])

  (* TODO: implement compare_str *)
  let str_binary op lhs rhs : ir =
    if Errors.has_errors () then
      error
    else
      let op =
        (match op with
        | Ast.EqOp -> Ir.Eq
        | NeqOp -> Ne
        | GtOp -> Gt
        | GeOp -> Ge
        | LtOp -> Lt
        | LeOp -> Le
        | _ -> Utils.Exn.unreachable ())
      in
      let open Ir in
      Cond (fun t f ->
        Cjump (op,
          Call (Name (Temp.named_label "compare_str"),
            [to_expr lhs; to_expr rhs]),
          Const 0,
          t, f))

  (* if two branches are both statements, no temp is need to store the results
  *)
  let if_stmt ~cond ~conseq ~alt =
    if Errors.has_errors () then
      error
    else
      let cond = to_cond cond in
      let conseq = to_stmt conseq in
      let alt = to_stmt alt in
      let t = Temp.new_label !temp_store in
      let f = Temp.new_label !temp_store in
      let z = Temp.new_label !temp_store in
      let open Ir in
      Stmt (seq [
        cond t f;
        Label t;
        conseq;
        Jump (Name z, [z]);
        Label f;
        alt;
        Label z;
      ])

  (* if at least one branch is Cond, handle them specially to avoid redundant jumps
  *)
  let if' ~cond ~conseq ~alt =
    if Errors.has_errors () then
      error
    else
      let cond = to_cond cond in
      match conseq, alt with
      | Cond conseq, Cond alt ->
        Cond (fun t f ->
          let t' = Temp.new_label !temp_store in
          let f' = Temp.new_label !temp_store in
          let open Ir in
          seq [
            cond t' f';
            Label t';
            conseq t f;
            Label f';
            alt t f;
          ])
      | Cond conseq, alt ->
        Cond (fun t f ->
          let t' = Temp.new_label !temp_store in
          let f' = Temp.new_label !temp_store in
          let open Ir in
          seq [
            cond t' f';
            Label t';
            conseq t f;
            Label f';
            to_stmt alt;
          ])
      | conseq, Cond alt ->
        Cond (fun t f ->
          let t' = Temp.new_label !temp_store in
          let f' = Temp.new_label !temp_store in
          let z = Temp.new_label !temp_store in
          let open Ir in
          seq [
            cond t' f';
            Label t';
            to_stmt conseq;
            Jump (Name z, [z]);
            Label f';
            alt t f;
            Label z;
          ])
      | _ ->
        Cond (fun t f ->
          let t' = Temp.new_label !temp_store in
          let f' = Temp.new_label !temp_store in
          let z = Temp.new_label !temp_store in
          let r = Temp.new_temp !temp_store in
          let open Ir in
          seq [
            cond t' f';
            Label t';
            Move (Temp r, to_expr conseq);
            Jump (Name z, [z]);
            Label f';
            Move (Temp r, to_expr alt);
            Label z;
          ])

end