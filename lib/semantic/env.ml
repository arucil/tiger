open Base
open Parse

type fun_type = Type.t list * Type.t

type t =
  {
    vars : Type.t Symtab.t;
    funs : fun_type Symtab.t;
    types : Type.t Symtab.t;
  }

let predefined =
  {
    vars = Symtab.empty;
    funs = (Symtab.extend_many Symtab.empty
      [
        (Symbol.sym "print", ([Type.StringType], Type.UnitType));
        (Symbol.sym "flush", ([], Type.UnitType));
        (Symbol.sym "getchar", ([], Type.StringType));
        (Symbol.sym "ord", ([Type.StringType], Type.IntType));
        (Symbol.sym "chr", ([Type.IntType], Type.StringType));
        (Symbol.sym "size", ([Type.StringType], Type.IntType));
        (Symbol.sym "substring", ([Type.StringType; Type.IntType; Type.IntType], Type.StringType));
        (Symbol.sym "concat", ([Type.StringType; Type.StringType], Type.StringType));
        (Symbol.sym "not", ([Type.IntType], Type.IntType));
        (Symbol.sym "exit", ([], Type.UnitType));
      ]);
    types = (Symtab.extend_many Symtab.empty
      [
        (Symbol.sym "int", Type.IntType);
        (Symbol.sym "string", Type.StringType);
      ]);
  }

let find_var { vars; _ } name =
  Symtab.find vars name

let find_fun { funs; _ } name =
  Symtab.find funs name

let find_type { types; _ } name =
  Symtab.find types name

let extend_var ({ vars; _ } as t) name ty =
  { t with vars = Symtab.extend vars name ty }

let extend_fun ({ funs; _ } as t) name ty =
  { t with funs = Symtab.extend funs name ty }

let extend_type ({ types; _ } as t) name ty =
  { t with types = Symtab.extend types name ty }