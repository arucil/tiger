open Common

type symbol = Symbol.t
  [@@deriving show]

type expr = Errors.pos * expr'
  [@@deriving show]

and expr' =
  | VarExpr of var
  | NilExpr
  | IntExpr of int32
  | StrExpr of string
  | CallExpr of { func : symbol; args: expr list }
  | BinaryExpr of { lhs : expr; op : op; rhs : expr }
  | UnaryExpr of { op : uop; rand : expr }
  | RecordExpr of { ty : symbol; fields : (Errors.pos * symbol * expr) list; }
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
  | SimpleVar of Errors.pos * symbol
  | FieldVar of Errors.pos * var * symbol
  | IndexVar of Errors.pos * var * expr
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
  | VarDecl of { name : symbol; escape : bool ref; ty : (Errors.pos * symbol) option; init : expr; pos : Errors.pos }
  | TypeDecl of typedecl list
  [@@deriving show]

and fundecl =
  { name : symbol; params : param list; ret : (Errors.pos * symbol) option; body : expr; pos : Errors.pos }
  [@@deriving show]

and param =
  { name : symbol; escape : bool ref; ty : symbol; pos : Errors.pos }
  [@@deriving show]

and typedecl =
  { name : symbol; ty : ty; pos : Errors.pos }
  [@@deriving show]

and ty =
  | AliasType of (Errors.pos * symbol)
  | RecordType of field list
  | ArrayType of (Errors.pos * symbol)
  [@@deriving show]

and field =
  { name : symbol; ty : symbol; pos : Errors.pos }
  [@@deriving show]