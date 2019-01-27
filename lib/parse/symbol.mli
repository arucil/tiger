open Base


type t

val sym : string -> t

val name : t -> string

module Format = Caml.Format

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int

val sexp_of_t : t -> Sexp.t

type comparator_witness

val comparator : (t, comparator_witness) Comparator.t
