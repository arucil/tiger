open Base
open Parse

type t

type fun_type = Type.t list * Type.t

type entry =
  | VarEntry of Type.t
  | FunEntry of fun_type

val predefined : t

val find_var : t -> Symbol.t -> entry option

val find_type : t -> Symbol.t -> Type.t option

val extend_var : t -> Symbol.t -> entry -> t

val extend_type : t -> Symbol.t -> Type.t -> t