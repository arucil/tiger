open Base

(** iterate on two lists, ignoring unequal length *)
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(** iterate on two lists with index passed to [f], ignoring unequal length *)
val iter2i : (int -> 'a -> 'b -> unit) -> 'a list -> 'b list -> unit

(** replace all elements satisfy [f] with [x'] in a list *)
val update : ('a -> bool) -> 'a -> 'a list -> 'a list

(** fold from right on two lists, ignoring unequal length *)
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'c -> 'a list -> 'b list -> 'c