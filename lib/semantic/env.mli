open Base
open Parse

module Make : functor (Translate : Translate.S) -> sig
  type t

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

  val predefined : t

  val find_var : t -> Symbol.t -> entry option

  val find_type : t -> Symbol.t -> Type.t option

  val extend_var : t -> Symbol.t -> entry -> t

  val extend_type : t -> Symbol.t -> Type.t -> t

end