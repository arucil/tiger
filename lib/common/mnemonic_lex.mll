{
  open Base

  type mpart =
    | Str of string
    | Src of int
    | Dest of int
    | Jump of int
}

let digit = ['0'-'9']

rule get_part =
  parse
  | "'s" (digit+ as s)
    {
      Some (Src (Int.of_string s))
    }
  | "'d" (digit+ as s)
    {
      Some (Dest (Int.of_string s))
    }
  | "'j" (digit+ as s)
    {
      Some (Jump (Int.of_string s))
    }
  | _+ as s
    {
      Some (Str s)
    }
  | eof { None }