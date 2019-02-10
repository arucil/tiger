open Base

(** iterate on two lists, ignoring unequal length *)
val iter2 : 'a list -> 'b list -> f:('a -> 'b -> unit) -> unit

(** iterate on two lists with index passed to [f], ignoring unequal length *)
val iter2i : 'a list -> 'b list -> f:(int -> 'a -> 'b -> unit) -> unit

(** replace all elements satisfy [f] with [x'] in a list *)
val update : 'a list -> 'a -> pred:('a -> bool) -> 'a list

(** fold from right on two lists, ignoring unequal lengths *)
val fold_right2 : 'a list -> 'b list -> init:'c -> f:('a -> 'b -> 'c -> 'c) -> 'c

(** ignore unequal lengths *)
val fold3 : 'b list -> 'c list -> 'd list -> init:'a -> f:('a -> 'b -> 'c -> 'd -> 'a) -> 'a

(* if not found, return -1 *)
val find_index : 'a list -> f:('a -> bool) -> int