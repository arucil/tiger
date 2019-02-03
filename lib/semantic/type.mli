open Base
open Parse

type unique

type unique_store

type t =
  | IntType
  | StringType
  | RecordType of (Symbol.t * t) list * unique
  | ArrayType of t * unique
  | NilType
  | UnitType
  | AliasType of { name : Symbol.t; mutable ty : t option }

val show : t -> string

val is_record : t -> bool

val is_array : t -> bool

val actual_type : t -> t

val is_compatible : t -> t -> bool

(** works on cyclic type *)
val (=) : t -> t -> bool

(** works on cyclic type *)
val (<>) : t -> t -> bool

val eq_ignore_unique : t -> t -> bool

val new_unique_store : unit -> unique_store

val new_unique : unique_store -> unique
