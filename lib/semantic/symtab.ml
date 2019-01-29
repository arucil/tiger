open Base
open Parse

type 'a t = (Symbol.t, 'a, Symbol.comparator_witness) Map.t

let empty = Map.empty (module Symbol)

let extend t sym data =
  Map.update t sym ~f:(fun _ -> data)

let rec extend_many t =
  List.fold ~init:t
    ~f:(fun t (sym, data) -> extend t sym data)

let find t key = Map.find t key