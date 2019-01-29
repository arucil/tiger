open Base
open Parse

type t

type fun_type = Type.t list * Type.t

val predefined : t

val find_var : t -> Symbol.t -> Type.t option

val find_fun : t -> Symbol.t -> fun_type option

val find_type : t -> Symbol.t -> Type.t option

val extend_var : t -> Symbol.t -> Type.t -> t

val extend_fun : t -> Symbol.t -> fun_type -> t

val extend_type : t -> Symbol.t -> Type.t -> t