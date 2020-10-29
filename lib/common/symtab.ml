open Base

type 'a t = (Symbol.t, 'a, Symbol.comparator_witness) Map.t

let empty = Map.empty (module Symbol)

let extend t sym data =
  Map.update t sym ~f:(fun _ -> data)

let extend_many t =
  List.fold ~init:t
    ~f:(fun t (sym, data) -> extend t sym data)

let find = Map.find

let remove = Map.remove