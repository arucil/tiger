open Base

type temp_store = int ref

type label = string

type temp = int

let new_store () = ref 0

let new_temp store =
  let i = !store in
  Int.incr store;
  i

let new_label store =
  let i = !store in
  Int.incr store;
  "L" ^ Int.to_string i

let named_label label = label