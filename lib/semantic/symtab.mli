open Base
open Parse

type 'a t

val empty : 'a t

(** If the symbol already exists, return None *)
val extend : 'a t -> Symbol.t -> 'a -> 'a t option

(** If the symbols already exist or duplicate, return None *)
val extend_many : 'a t -> (Symbol.t * 'a) list -> 'a t option

val find : 'a t -> Symbol.t -> 'a option