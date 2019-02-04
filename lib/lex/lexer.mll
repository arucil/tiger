{
  open Lexing
  open Base
  open Token

  exception Error of Errors.pos * string

  let get_pos lexbuf =
    let pos = lexbuf.lex_start_p in
    (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with
        pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let whitespace = [' ' '\t' '\r']+


rule get_token =
  parse
  | whitespace { get_token lexbuf }
  | '\n' { next_line lexbuf; get_token lexbuf }
  | "while" { WHILE (get_pos lexbuf)}
  | "for" { FOR (get_pos lexbuf) }
  | "to" { TO (get_pos lexbuf) }
  | "break" { BREAK (get_pos lexbuf) }
  | "let" { LET (get_pos lexbuf) }
  | "in" { IN (get_pos lexbuf) }
  | "end" { END (get_pos lexbuf) }
  | "function" { FUNCTION (get_pos lexbuf) }
  | "var" { VAR (get_pos lexbuf) }
  | "type" { TYPE (get_pos lexbuf) }
  | "array" { ARRAY (get_pos lexbuf) }
  | "if" { IF (get_pos lexbuf) }
  | "then" { THEN (get_pos lexbuf) }
  | "else" { ELSE (get_pos lexbuf) }
  | "do" { DO (get_pos lexbuf) }
  | "of" { OF (get_pos lexbuf) }
  | "nil" { NIL (get_pos lexbuf) }
  | alpha (alpha | digit)* as s
    { ID ((get_pos lexbuf), s) }
  | digit+ as s
    { INT (get_pos lexbuf, Int.of_string s) }
  | ',' { COMMA (get_pos lexbuf) }
  | ':' { COLON (get_pos lexbuf) }
  | ';' { SEMI (get_pos lexbuf) }
  | '(' { LPAREN (get_pos lexbuf) }
  | ')' { RPAREN (get_pos lexbuf) }
  | '[' { LBRACK (get_pos lexbuf) }
  | ']' { RBRACK (get_pos lexbuf) }
  | '{' { LBRACE (get_pos lexbuf) }
  | '}' { RBRACE (get_pos lexbuf) }
  | '.' { DOT (get_pos lexbuf) }
  | '+' { PLUS (get_pos lexbuf) }
  | '-' { MINUS (get_pos lexbuf) }
  | '*' { TIMES (get_pos lexbuf) }
  | '/' { DIV (get_pos lexbuf) }
  | '=' { EQ (get_pos lexbuf) }
  | "<>" { NEQ (get_pos lexbuf) }
  | '>' { GT (get_pos lexbuf) }
  | ">=" { GE (get_pos lexbuf) }
  | '<' { LT (get_pos lexbuf) }
  | "<=" { LE (get_pos lexbuf) }
  | '&' { AND (get_pos lexbuf) }
  | '|' { OR (get_pos lexbuf) }
  | ":=" { ASSIGN (get_pos lexbuf) }
  | "/*" { read_comment 0 lexbuf }
  | '"' { read_string (get_pos lexbuf) (Buffer.create 10) lexbuf }
  | _ as c
    {
      raise (Error
        (get_pos lexbuf,
         Printf.sprintf "unexpected char: %c (%d)" c (Char.to_int c)))
    }
  | eof { EOF (get_pos lexbuf) }

and read_string start_pos buf =
  parse
  | '"' { STR (start_pos, Buffer.contents buf) }
  | '\\' { read_escape start_pos buf lexbuf }
  | _ as c { Buffer.add_char buf c; read_string start_pos buf lexbuf }
  | '\n'
    {
      next_line lexbuf;
      raise (Error (get_pos lexbuf, "unclosed string"))
    }
  | eof
    {
      raise (Error (get_pos lexbuf, "unclosed string"))
    }

and read_escape start_pos buf =
  parse
  | 'n' { Buffer.add_char buf '\n'; read_string start_pos buf lexbuf }
  | 't' { Buffer.add_char buf '\t'; read_string start_pos buf lexbuf }
  | '"' { Buffer.add_char buf '"'; read_string start_pos buf lexbuf }
  | '\\' { Buffer.add_char buf '\\'; read_string start_pos buf lexbuf }
  | digit digit digit as s
    {
      let n = Int.of_string s in
      if n < 128 then begin
          Buffer.add_char buf (Char.of_int_exn n);
          read_string start_pos buf lexbuf
        end
      else
        raise (Error (get_pos lexbuf, "ASCII code out of range"))
    }
  | '\n'
    { next_line lexbuf; read_linespan_escape start_pos buf lexbuf }
  | ['\x00'-' ']
    { read_linespan_escape start_pos buf lexbuf }
  | _
    { raise (Error (get_pos lexbuf, "invalid escape")) }
  | eof
    { raise (Error (get_pos lexbuf, "unclosed string")) }

and read_linespan_escape start_pos buf =
  parse
  | '\\'
    { read_string start_pos buf lexbuf }
  | '\n'
    { next_line lexbuf; read_linespan_escape start_pos buf lexbuf }
  | ['\x00'-' ']
    { read_linespan_escape start_pos buf lexbuf }
  | _ { raise (Error (get_pos lexbuf, "invalid escape")) }
  | eof { raise (Error (get_pos lexbuf, "unclosed string")) }

and read_comment level =
  parse
  | "*/"
    {
      if level = 0 then
        get_token lexbuf
      else
        read_comment (level - 1) lexbuf
    }
  | "/*"
    {
      read_comment (level + 1) lexbuf
    }
  | '\n'
    { next_line lexbuf; read_comment level lexbuf }
  | _
    { read_comment level lexbuf }
  | eof
    { raise (Error (get_pos lexbuf, "unclosed comment")) }

