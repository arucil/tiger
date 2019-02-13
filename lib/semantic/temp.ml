open Base
open Parse

module Format = Caml.Format

type temp_store = int ref

type label = Symbol.t [@@deriving show]

type temp = int

let new_store () = ref 0

let new_temp store =
  let i = !store in
  Int.incr store;
  i

let new_label store =
  let i = !store in
  Int.incr store;
  Symbol.sym ("L" ^ Int.to_string i)

let new_labels n store =
  List.init n ~f:(fun _ -> new_label store)

let named_label label = Symbol.sym label


let pp_temp (fmt : Format.formatter) temp =
  Format.pp_print_string fmt ("t" ^ Int.to_string temp)