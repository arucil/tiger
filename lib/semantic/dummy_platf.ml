open Base
open Parse

type access =
  | InFrame of int
  | InReg of Temp.temp

(* TODO: pass temp_store? *)
let fp = Temp.new_temp (Temp.new_store ())

let access_expr access base =
  match access with
  | InFrame offset ->
    let open Ir in
    Mem (Binop (Add, Temp (fp), Const (offset)))
  | InReg reg -> Temp reg

module Frame = struct

  type t =
    {
      label : Temp.label;
      params : access list;
      (*
      TODO:
      instructions required to 'view shift'
      number of locals
      *)
    }

  let new_frame label params temp_store =
    {
      label;
      params = List.map params
      (**
      TODO:
      params根据escape指定reg或stack,
      要生成view shift用的指令，把caller的参数(前k个用reg,其他用stack)
      转移到param所用的reg或stack
      *)
        ~f:(fun escape ->
          if escape then InFrame 0
          else InReg (Temp.new_temp temp_store))
    }

  let label t = t.label

  let params t = t.params

  let new_local t escape temp_store =
    if escape then InFrame 0
    else InReg (Temp.new_temp temp_store)

  let view_shift t stmt = stmt

end


(* TODO: pass temp_store? *)
let rv = Temp.new_temp (Temp.new_store ())

let word_size = 4


type fragment =
  | Fun of Frame.t * Ir.stmt
  | Str of Temp.label * string

let string_lit label str = Str (label, str)

let fun' frame stmt = Fun (frame, stmt)

let external_call name args =
  let open Ir in
  Call (Name (Symbol.sym name), args)