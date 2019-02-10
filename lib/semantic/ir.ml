open Base

type expr =
  | Const of int
  | Name of Temp.label
  | Temp of Temp.temp
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Mem of expr
  | Call of expr * expr list
  | Eseq of stmt * expr

and stmt =
  | Move of expr * expr
  | Expr of expr
  | Jump of expr * Temp.label list
  | Cjump of relop * expr * expr * Temp.label * Temp.label
  | Seq of stmt * stmt
  | Label of Temp.label

and binop =
  | Add | Sub | Mul | Div
  | And | Or | Lshift | Rshift | Arshift | Xor

and unop =
  | Neg
  | Not

and relop =
  | Eq | Ne | Lt | Le | Gt | Ge
  | Ult | Ule | Ugt | Uge

(** build a chain of Seq's *)
let rec seq : stmt list -> stmt = function
  | [] -> raise (Invalid_argument "seq")
  | [stmt] -> stmt
  | (stmt :: stmts) ->
    Seq (stmt, seq stmts)