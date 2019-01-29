open Base
open Parse

type unique

val new_unique : unit -> unique

type t =
  | IntType
  | StringType
  | RecordType of (Symbol.t * t) list * unique
  | ArrayType of t * unique
  | NilType
  | UnitType
  | AliasType of Symbol.t * t option ref

val show : t -> string

val is_record : t -> bool

val is_array : t -> bool

val actual_type : t -> t

val is_compatible : t -> t -> bool