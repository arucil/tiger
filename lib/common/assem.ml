open Base

type mnemonic = mtoken array
and mtoken =
  | Str of string
  | Src of int
  | Dst of int
  | Jump of int

type t =
  | Oper of {
      assem : mnemonic;
      dst : Temp.temp array;
      src : Temp.temp array;
      jumps : Temp.label array
    }
  | Label of { assem : mnemonic; label : Temp.label }
  | Move of {
      assem : mnemonic;
      dst : Temp.temp;
      src : Temp.temp;
    }

let show_mnemonic m fmt_temp dest src jumps =
  let buf = Buffer.create 10 in
  Array.iter m
    ~f:(function
      | Str s -> Buffer.add_string buf s
      | Src i -> fmt_temp (Array.get src i) |> Buffer.add_string buf
      | Dst i -> fmt_temp (Array.get dest i) |> Buffer.add_string buf
      | Jump i -> Temp.show_label (Array.get jumps i) |> Buffer.add_string buf);
  Buffer.contents buf

let show t fmt_temp =
  match t with
  | Oper { assem; dst; src; jumps } ->
    show_mnemonic assem fmt_temp dst src jumps
  | Label { assem; label } ->
    show_mnemonic assem fmt_temp [||] [||] [|label|]
  | Move { assem; dst; src } ->
    show_mnemonic assem fmt_temp [|dst|] [|src|] [||]

let parse_int s len pos =
  let rec find_int_end i =
    if i < len && Char.is_digit (String.get s i) then
      find_int_end (i + 1)
    else
      i
  in
  let end' = find_int_end pos in
  (Int.of_string (String.sub s ~pos ~len:(end' - pos)), end')

let rec parse_mnemonic s len pos =
  if pos = len - 1 then
    []
  else
    match String.index_from s pos '\'' with
    | None -> [Str (String.subo s ~pos)]
    | Some pos' ->
      if pos' > len - 3 then
        [Str (String.subo s ~pos)]
      else if Char.is_digit (String.get s (pos' + 2)) then
        let (n, pos'') = parse_int s len (pos' + 2) in
        begin
          match String.get s (pos' + 1) with
          | 's' ->
            Str (String.sub s pos (pos' - pos)) :: Src n :: parse_mnemonic s len pos''
          | 'd' ->
            Str (String.sub s pos (pos' - pos)) :: Dst n :: parse_mnemonic s len pos''
          | 'j' ->
            Str (String.sub s pos (pos' - pos)) :: Jump n :: parse_mnemonic s len pos''
          | _ -> parse_mnemonic s len pos''
        end
      else
        parse_mnemonic s len (pos' + 1)

let make_mnemonic fmt =
  Printf.ksprintf
    (fun s -> parse_mnemonic s (String.length s) 0 |> Array.of_list)
    fmt