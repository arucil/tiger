open Base
open Parse

module Format = Caml.Format

type temp_store =
  {
    mutable temp_counter : int;
    mutable label_counter : int;
  }

type label = Symbol.t [@@deriving show]

type temp = int

let new_store () =
  {
    temp_counter = 100;
    label_counter = 0;
  }

let new_temp store =
  let i = store.temp_counter in
  store.temp_counter <- i + 1;
  i

let temp_of_int n = n

let new_label store =
  let i = store.label_counter in
  store.label_counter <- i + 1;
  Symbol.sym ("_L" ^ Int.to_string i)

let new_labels n store =
  List.init n ~f:(fun _ -> new_label store)

let named_label = Symbol.sym


let pp_temp (fmt : Format.formatter) temp =
  Format.pp_print_string fmt ("t" ^ Int.to_string temp)