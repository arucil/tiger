open Base
open Parse

type ir =
  | Expr of Ir.expr
  | Stmt of Ir.stmt
  | Cond of (Temp.label -> Temp.label -> Ir.stmt)

module type S = sig
  type level
  type access

  type fragment

  val set_temp_store : Temp.temp_store -> unit

  val outermost : level

  val new_level : level -> Temp.label -> bool list -> level

  val params : level -> access list

  val new_local : level -> bool -> access

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

  val assign : var:ir -> expr:ir -> ir

  val if_stmt : cond:ir -> conseq:ir -> alt:ir -> ir

  val if' : cond:ir -> conseq:ir -> alt:ir -> ir

  val record : ir list -> ir

  val array : size:ir -> init:ir -> ir

  val break : Temp.label -> ir

  val while' : break:Temp.label -> cond:ir -> body:ir -> ir

  val for' : break:Temp.label -> access -> low:ir -> high:ir -> body:ir -> ir

  val call : fun_level:level -> use_level:level -> Symbol.t -> ir list -> ir

  val let' : inits:ir list -> body:ir -> ir

  val init_var : access -> ir -> ir

  val fun' : level -> ir -> unit

  val fragments : unit -> fragment list

end


(* TODO: Exercise 6.5 eliminate unnecessary static links *)
(* TODO: floating-point numbers *)
(* TODO: value of non-word-size *)

module Make (Platf : Platform.S) = struct

  type level =
    {
      parent : level option;
      frame : Platf.Frame.t;
    }

  and access = level * Platf.access

  type fragment = Platf.fragment

  let temp_store = ref (Temp.new_store ())

  let set_temp_store store = temp_store := store

  let outermost =
    let temp_store = Temp.new_store () in
    {
      parent = None;
      frame = Platf.Frame.new_frame (Temp.named_label "main") [] temp_store
    }

  let new_level parent label params =
    {
      parent = Some parent;
      frame = Platf.Frame.new_frame label (true :: params) !temp_store;
    }

  let params level =
    Platf.Frame.params level.frame
      |> List.tl_exn
      |> List.map ~f:(fun acc -> (level, acc))

  let static_link level =
    List.hd_exn (Platf.Frame.params level.frame)

  let build_static_links ~def_level ~use_level =
    let rec go level expr =
      if phys_equal level def_level then
        expr
      else
        go (Option.value_exn level.parent)
          (Platf.access_expr (static_link level) expr)
    in
      go use_level (Temp Platf.fp)

  let new_local level escape =
    (level, Platf.Frame.new_local level.frame escape !temp_store)


  let fragment_list : Platf.fragment list ref = ref []


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
      Expr (Platf.access_expr access (build_static_links ~def_level ~use_level))
  
  (* TODO: emit code that check nil *)
  let field_var ~record sym fields =
    if Errors.has_errors () then
      error
    else
      let i = Utils.List.find_index fields
        ~f:(fun (sym', _) -> Symbol.(=) sym sym')
      in
      let open Ir in
      Expr (Mem (Binop (Add, to_expr record, Binop (Mul, Const i, Const Platf.word_size))))

  (* TODO: emit code that check out-of-bound *)
  let index_var ~array ~index =
    if Errors.has_errors () then
      error
    else
      let open Ir in
      Expr (Mem (Binop (Add, to_expr array, Binop (Mul, to_expr index, Const Platf.word_size))))

  let int n : ir =
    let open Ir in
    Expr (Const n)

  let str s =
    if Errors.has_errors () then
      error
    else
      let label = Temp.new_label !temp_store in
      let frag = Platf.string_lit label s in
      fragment_list := !fragment_list @ [frag];
      Expr (Ir.Name label)

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
          Platf.external_call "compare_str" [to_expr lhs; to_expr rhs],
          Const 0,
          t, f))

  let assign ~var ~expr : ir =
    if Errors.has_errors () then
      error
    else
      let open Ir in
      Stmt (Move (to_expr var, to_expr expr))

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
        let r = Temp.new_temp !temp_store in
        let t = Temp.new_label !temp_store in
        let f = Temp.new_label !temp_store in
        let z = Temp.new_label !temp_store in
        let open Ir in
        Expr (Eseq (
          seq [
            cond t f;
            Label t;
            Move (Temp r, to_expr conseq);
            Jump (Name z, [z]);
            Label f;
            Move (Temp r, to_expr alt);
            Label z;
          ],
          Temp r
        ))

  let record fields : ir =
    if Errors.has_errors () then
      error
    else
      let r = Temp.new_temp !temp_store in
      let open Ir in
      Expr (Eseq (
        seq (
          Move (Temp r, Platf.external_call "alloc_record" [Const (List.length fields)])
          ::
          List.mapi fields
            ~f:(fun i field ->
              Move (
                Mem (Binop (Add, Temp r, Const (i * Platf.word_size))),
                to_expr field))
        ),
        Temp r
      ))

  let array ~size ~init : ir =
    if Errors.has_errors () then
      error
    else
      Expr (Platf.external_call "alloc_array" [to_expr size; to_expr init])

  let break label : ir =
    if Errors.has_errors () then
      error
    else
      Stmt (Ir.Jump (Name label, [label]))

  let while' ~break ~cond ~body : ir =
    if Errors.has_errors () then
      error
    else
      let cond = to_cond cond in
      let test = Temp.new_label !temp_store in
      let t = Temp.new_label !temp_store in
      let open Ir in
      Stmt (seq [
        Label test;
        cond t break;
        Label t;
        to_stmt body;
        Jump (Name test, [test]);
        Label break;
      ])

  let for' ~break (_, access) ~low ~high ~body : ir =
    if Errors.has_errors () then 
      error
    else
      let i = Platf.access_expr access (Temp Platf.fp) in
      let limit = Ir.Temp (Temp.new_temp !temp_store) in
      let t = Temp.new_label !temp_store in
      let test = Temp.new_label !temp_store in
      let open Ir in
      Stmt (seq [
        Move (i, to_expr low);
        Move (limit, to_expr high);
        Label test;
        Cjump (Le, i, limit, t, break);
        Label t;
        to_stmt body;
        Move (i, Binop (Add, i, Const 1));
        Jump (Name test, [test]);
        Label break;
      ])

  (* TODO: stdlib函数和自定义函数的调用方式不同？ *)
  let call ~fun_level ~use_level name args : ir =
    if Errors.has_errors () then
      error
    else
      let def_level = Option.value_exn (fun_level.parent) in
      let open Ir in
      Expr (Call (Name name, build_static_links ~def_level ~use_level :: List.map args ~f:to_expr))

  let let' ~inits ~body : ir =
    if Errors.has_errors () then
      error
    else
      Stmt (Ir.seq (List.map (inits @ [body]) ~f:to_stmt))

  let init_var (_, access) init : ir =
    if Errors.has_errors () then
      error
    else
      let open Ir in
      Stmt (Move (Platf.access_expr access (Temp Platf.fp), to_expr init))

  let fun' level body =
    let body = Ir.Move (Ir.Temp Platf.rv, to_expr body) in
    let body = Platf.Frame.view_shift level.frame body in
    let frag = Platf.fun' level.frame body in
    fragment_list := !fragment_list @ [frag]

  let fragments () = !fragment_list

end