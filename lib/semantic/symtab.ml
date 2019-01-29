open Base
open Parse

type 'a t = (Symbol.t, 'a, Symbol.comparator_witness) Map.t

let empty = Map.empty (module Symbol)

let extend t key data =
  match Map.add t ~key ~data with
  | `Duplicate -> None
  | `Ok t' -> Some t'

let rec extend_many t = function
  | [] -> Some t
  | ((key, data) :: xs) ->
    match extend t key data with
    | None -> None
    | Some t' -> extend_many t' xs

let find t key = Map.find t key