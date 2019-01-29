open Base
open Parse

type 'a t

val empty : 'a t

val extend : 'a t -> Symbol.t -> 'a -> 'a t

val extend_many : 'a t -> (Symbol.t * 'a) list -> 'a t

val find : 'a t -> Symbol.t -> 'a option