open Base
open Parse

type 'a t

val empty : 'a t

val extend : 'a t -> Symbol.t -> 'a -> 'a t option

val find : 'a t -> Symbol.t -> 'a option