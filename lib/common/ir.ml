open Base

module Format = Caml.Format

type expr =
  | Const of int32
  | Name of Temp.label
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Call of expr * expr list
  | Eseq of stmt * expr
  | Lval of lval
  [@@deriving show]

and lval =
  | Temp of Temp.temp
  | Mem of expr
  [@@deriving show]

and stmt =
  | Move of lval * expr
  | Expr of expr
  | Jump of expr * Temp.label list
  | Cjump of relop * expr * expr * Temp.label * Temp.label
  | Seq of stmt * stmt
  | Label of Temp.label
  [@@deriving show]

and binop =
  | Add | Sub | Mul | Div
  | And | Or | Xor | Lshift | Rshift | Arshift
  [@@deriving show]

and unop =
  | Neg
  | Not
  [@@deriving show]

and relop =
  | Eq | Ne | Lt | Le | Gt | Ge
  | Ult | Ule | Ugt | Uge
  [@@deriving show]

(** build a chain of Seq's *)
let rec seq : stmt list -> stmt = function
  | [] -> raise (Invalid_argument "seq")
  | [stmt] -> stmt
  | (stmt :: stmts) ->
    Seq (stmt, seq stmts)

let negate_relop = function
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Le -> Gt
  | Gt -> Le
  | Ge -> Lt
  | Ult -> Uge
  | Ule -> Ugt
  | Ugt -> Ule
  | Uge -> Ult