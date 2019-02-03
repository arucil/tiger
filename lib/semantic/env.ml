open Base
open Parse

type fun_type = Type.t list * Type.t

type var_type = Type.t * bool

type entry =
  | VarEntry of var_type
  | FunEntry of fun_type

type t =
  {
    vars : entry Symtab.t;
    types : Type.t Symtab.t;
  }

let predefined =
  {
    vars = (Symtab.extend_many Symtab.empty
      [
        (Symbol.sym "print", FunEntry ([Type.StringType], Type.UnitType));
        (Symbol.sym "flush", FunEntry ([], Type.UnitType));
        (Symbol.sym "getchar", FunEntry ([], Type.StringType));
        (Symbol.sym "ord", FunEntry ([Type.StringType], Type.IntType));
        (Symbol.sym "chr", FunEntry ([Type.IntType], Type.StringType));
        (Symbol.sym "size", FunEntry ([Type.StringType], Type.IntType));
        (Symbol.sym "substring", FunEntry ([Type.StringType; Type.IntType; Type.IntType], Type.StringType));
        (Symbol.sym "concat", FunEntry ([Type.StringType; Type.StringType], Type.StringType));
        (Symbol.sym "not", FunEntry ([Type.IntType], Type.IntType));
        (Symbol.sym "exit", FunEntry ([], Type.UnitType));
      ]);
    types = (Symtab.extend_many Symtab.empty
      [
        (Symbol.sym "int", Type.IntType);
        (Symbol.sym "string", Type.StringType);
      ]);
  }

let find_var { vars; _ } name =
  Symtab.find vars name

let find_type { types; _ } name =
  Symtab.find types name

let extend_var ({ vars; _ } as t) name ty =
  { t with vars = Symtab.extend vars name ty }

let extend_type ({ types; _ } as t) name ty =
  { t with types = Symtab.extend types name ty }