
open Lex.Token


type symbol = Symbol.t
 [@@deriving show]

type expr = token_pos * expr'
  [@@deriving show]

and expr' =
  | VarExpr of var
  | NilExpr
  | IntExpr of int
  | StrExpr of string
  | CallExpr of { func : symbol; args: expr list }
  | BinaryExpr of { lhs : expr; op : op; rhs : expr }
  | UnaryExpr of { op : uop; rand : expr }
  | RecordExpr of { ty : symbol; fields : (token_pos * symbol * expr) list; }
  | ArrayExpr of { ty : symbol; size : expr; init : expr }
  | SeqExpr of expr list
  | AssignExpr of { var : var; expr : expr }
  | IfExpr of { cond : expr; conseq : expr; alt : expr option }
  | WhileExpr of { cond : expr; body : expr }
  | ForExpr of { var : symbol; escape : bool ref; low : expr; high : expr; body : expr }
  | BreakExpr
  | LetExpr of { decls : decl list; body : expr }
  [@@deriving show]

and var =
  | SimpleVar of token_pos * symbol
  | FieldVar of token_pos * var * symbol
  | IndexVar of token_pos * var * expr
  [@@deriving show]

and op =
  | AddOp | SubOp | MulOp | DivOp
  | EqOp | NeqOp | GtOp | GeOp | LtOp | LeOp
  | AndOp | OrOp
  [@@deriving show]

and uop =
  | NegOp
  [@@deriving show]

and decl =
  | FunDecl of fundecl list
  | VarDecl of { name : symbol; escape : bool ref; ty : (token_pos * symbol) option; init : expr; pos : token_pos }
  | TypeDecl of typedecl list
  [@@deriving show]

and fundecl =
  { name : symbol; params : param list; ret : (token_pos * symbol) option; body : expr; pos : token_pos }
  [@@deriving show]

and param =
  { name : symbol; escape : bool ref; ty : symbol; pos : token_pos }
  [@@deriving show]

and typedecl =
  { name : symbol; ty : ty; pos : token_pos }
  [@@deriving show]

and ty =
  | NameType of (token_pos * symbol)
  | RecordType of field list
  | ArrayType of (token_pos * symbol)
  [@@deriving show]

and field =
  { name : symbol; ty : symbol; pos : token_pos }
  [@@deriving show]