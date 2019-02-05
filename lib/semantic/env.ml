open Base
open Parse

module Make (Translate : Translate.S) = struct

  type fun_type =
    {
      label : Temp.label;
      level : Translate.level;
      params : Type.t list;
      ret : Type.t
    }

  type var_type =
    {
      access : Translate.access;
      ty : Type.t;
      assignable : bool
    }

  type entry =
    | VarEntry of var_type
    | FunEntry of fun_type

  type t =
    {
      vars : entry Symtab.t;
      types : Type.t Symtab.t;
    }

  let predefined =
    let fn name params ret =
      (Symbol.sym name,
       FunEntry
         {
           label = Temp.named_label name;
           level = Translate.outermost;
           params;
           ret;
         })
    in
    {
      vars = (Symtab.extend_many Symtab.empty
        [
          fn "print" [Type.StringType] Type.UnitType;
          fn "flush" [] Type.UnitType;
          fn "getchar" [] Type.StringType;
          fn "ord" [Type.StringType] Type.IntType;
          fn "chr" [Type.IntType] Type.StringType;
          fn "size" [Type.StringType] Type.IntType;
          fn "substring" [Type.StringType; Type.IntType; Type.IntType] Type.StringType;
          fn "concat" [Type.StringType; Type.StringType] Type.StringType;
          fn "not" [Type.IntType] Type.IntType;
          fn "exit" [] Type.UnitType;
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

end