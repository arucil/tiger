open Base

type t = int

let symbols = Hashtbl.create (module String)

let symbol_names = Hashtbl.create (module Int)

let symbol_counter = ref 0

let sym s =
  Hashtbl.find_or_add symbols s
    ~default:(fun () ->
      let i = !symbol_counter in
      Int.incr symbol_counter;
      Hashtbl.add_exn symbol_names ~key:i ~data:s;
      i)


let name = Hashtbl.find_exn symbol_names


module Format = Caml.Format

let pp (fmt : Format.formatter) t =
  Format.pp_print_string fmt (name t)

let show t =
  Format.sprintf "\"%s\"" (name t)