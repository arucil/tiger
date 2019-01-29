open Base
open Parse

type unique = int

let unique_counter = ref 0

let new_unique () =
  let i = !unique_counter in
  Int.incr unique_counter;
  i

type t =
  | IntType
  | StringType
  | RecordType of (Symbol.t * t) list * unique
  | ArrayType of t * unique
  | NilType
  | UnitType
  | AliasType of Symbol.t * t option ref