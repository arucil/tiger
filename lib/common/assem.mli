
type mnemonic

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
      src : Temp.temp
    }

val show : t -> (Temp.temp -> string) -> string

val make_mnemonic : ('a, unit, string, mnemonic) format4 -> 'a