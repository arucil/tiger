open Base
open Parse

type temp_store = int ref

type label = Symbol.t

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

let named_label label = Symbol.sym label