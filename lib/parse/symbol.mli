open Base

type t

val sym : string -> t

val name : t -> string

module Format = Caml.Format

val pp : Format.formatter -> t -> unit
