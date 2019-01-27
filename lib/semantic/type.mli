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