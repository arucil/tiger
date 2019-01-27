open Base
open Parse

type 'a t = Map

let empty = Map.empty (module Symbol)

let extend t key data =
  match Map.add t ~key ~data with
  | `Duplicate -> None
  | `Ok t' -> Some t'

let find t key = Map.find t key