open Base
open Parse

type access =
  | InFrame of int
  | InReg of Temp.temp

let fp = Temp.temp_of_int 30   (* $fp in MIPS *)

let rv = Temp.temp_of_int 2    (* $v0 in MIPS *)

let access_expr access base =
  match access with
  | InFrame offset ->
    let open Ir in
    Mem (Binop (Add, base, Const (offset)))
  | InReg reg -> Temp reg

let word_size = 4

(* $a0 ~ $a3 in MIPS *)
let reg_params =
  Array.init 4 ~f:(fun i -> Temp.temp_of_int (i + 4))

module Frame = struct

  type t =
    {
      label : Temp.label;
      params : access list;
      mutable next_offset : int;
      mutable entry_view_shift : Ir.stmt list;
      mutable exit_view_shift : Ir.stmt list;
    }

  let new_frame label params temp_store =
    let entry_view_shift = ref [] in
    let exit_view_shift = ref [] in
    let next_offset = ref (-word_size) in
    {
      label;
      params = List.mapi params
      (**
      TODO:
      params根据escape指定reg或stack,
      要生成view shift用的指令，把caller的参数(前k个用reg,其他用stack)
      转移到param所用的reg或stack
      *)
        ~f:(fun i escape ->
          let src =
            if i < Array.length reg_params then
              Ir.Temp (reg_params.(i))
            else
              let open Ir in
              (* fp 已经更新,指向arguments底部 *)
              Binop (Add, Temp fp, Const ((i - Array.length reg_params) * word_size))
          in
          let dest =
            if escape then
              (let offset = !next_offset in
               next_offset := offset - word_size;
               InFrame offset)
            else
              InReg (Temp.new_temp temp_store)
          in
          entry_view_shift := !entry_view_shift
            @ [let open Ir in Move (access_expr dest (Temp fp), src)];
          dest);
      next_offset = !next_offset;
      entry_view_shift = !entry_view_shift;
      exit_view_shift = !exit_view_shift;
    }

  let label t = t.label

  let params t = t.params

  let new_local t escape temp_store =
    if escape then
      (let offset = t.next_offset in
       t.next_offset <- offset - word_size;
       InFrame offset)
    else
      InReg (Temp.new_temp temp_store)

  let view_shift t stmt =
    let open Ir in
    seq (t.entry_view_shift @ [stmt])

end


type fragment =
  | Fun of Frame.t * Ir.stmt
  | Str of Temp.label * string

let string_lit label str = Str (label, str)

let fun' frame stmt = Fun (frame, stmt)

let external_call name args =
  let open Ir in
  Call (Name (Symbol.sym name), args)