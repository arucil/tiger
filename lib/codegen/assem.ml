open Base
open Common

module Mn = Mnemonic_lex

type mnemonic = Mn.mpart array

type t =
  | Oper of {
      assem : mnemonic;
      dest : Temp.temp array;
      src : Temp.temp array;
      jumps : Temp.label array
    }
  | Label of { assem : mnemonic; label : Temp.label }
  | Move of { assem : mnemonic; dest : Temp.temp; src : Temp.temp }

let show_mnemonic m fmt_temp dest src jumps =
  let buf = Buffer.create 10 in
  Array.iter m
    ~f:(function
      | Mn.Str s -> Buffer.add_string buf s
      | Src i -> fmt_temp (Array.get src i) |> Buffer.add_string buf
      | Dest i -> fmt_temp (Array.get dest i) |> Buffer.add_string buf
      | Jump i -> Temp.show_label (Array.get jumps i) |> Buffer.add_string buf);
  Buffer.contents buf

let show t fmt_temp =
  match t with
  | Oper { assem; dest; src; jumps } ->
    show_mnemonic assem fmt_temp dest src jumps
  | Label { assem; label } ->
    show_mnemonic assem fmt_temp [||] [||] [|label|]
  | Move { assem; dest; src } ->
    show_mnemonic assem fmt_temp [|dest|] [|src|] [||]

let make_mnemonic s =
  let lexbuf = Lexing.from_string s in
  let rec go () =
    match Mn.get_part lexbuf with
    | None -> []
    | Some p ->
      p :: go ()
  in
    Array.of_list (go ())