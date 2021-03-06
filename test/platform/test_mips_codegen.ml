open Base
open OUnit2
open Lex
open Parse
open Semantic
open Canonical
open Stdio
open Platform
open Common

let show_temp t =
  match Temp.temp_to_int t with
  | 0 -> "$0"
  | 1 -> "$at"
  | 2 -> "$v0"
  | 3 -> "$v1"
  | 4 -> "$a0"
  | 5 -> "$a1"
  | 6 -> "$a2"
  | 7 -> "$a3"
  | 8 -> "$t0"
  | 9 -> "$t1"
  | 10 -> "$t2"
  | 11 -> "$t3"
  | 12 -> "$t4"
  | 13 -> "$t5"
  | 14 -> "$t6"
  | 15 -> "$t7"
  | 16 -> "$s0"
  | 17 -> "$s1"
  | 18 -> "$s2"
  | 19 -> "$s3"
  | 20 -> "$s4"
  | 21 -> "$s5"
  | 22 -> "$s6"
  | 23 -> "$s7"
  | 24 -> "$t8"
  | 25 -> "$t9"
  | 26 -> "$k0"
  | 27 -> "$k1"
  | 28 -> "$gp"
  | 29 -> "$sp"
  | 30 -> "$fp"
  | 31 -> "$ra"
  | _ -> Temp.show_temp t

let show_assems asm = List.map ~f:(fun a -> Assem.show a show_temp) asm |> String.concat ~sep:"\n"

let trim_assems asm = String.lstrip ~drop:(fun c -> Char.(c = '\n')) asm
  |> String.rstrip

type frag =
  | F of string
  | S of string

let frag_eq x y =
  match x, y with
  | F asm1, F asm2 -> String.(asm1 = trim_assems asm2)
  | S s1, S s2 -> String.(s1 = s2)
  | _ -> false

let show_frag = function
  | F asm -> asm
  | S s -> Printf.sprintf {|"%s"|} s

let canonicalize stmt temp_store =
  let stmts = Canon.linearize stmt temp_store in
  let (blocks, done_label) = Canon.basic_blocks stmts temp_store in
  Canon.trace_schedule blocks done_label temp_store

let run_codegen s =
  let temp_file = Caml.Filename.temp_file "tiger_canonical_" "" in
  let oc = Out_channel.create ~append:false temp_file in
  Errors.set_out oc;
  Errors.set_source_name "-";
  let expr = Parser.prog Lexer.get_token (Lexing.from_string s) in
  let trans = (module Translate.Make(Mips_platf) : Translate.S with type fragment = Mips_platf.fragment) in
  let temp_store = Temp.new_store () in
  let (_, stmt) = Analyzer.trans_prog expr (module (val trans) : Translate.S) temp_store in
  Out_channel.close oc;

  let errors = In_channel.read_all temp_file in

  let codegen stmts = Codegen.Gen.gen (module Mips_gen) stmts temp_store in
  let stmt2assems stmt =
    canonicalize stmt temp_store
      |> codegen
      |> show_assems
  in

  let module Trans = (val trans) in
  let frags = List.map (Trans.fragments ())
    ~f:(function
      | Fun (frame, stmt) ->
        (Mips_platf.Frame.label frame, F (stmt2assems stmt))
      | Str (label, str) ->
        (label, S str))
    |> Map.of_alist_exn (module Symbol)
  in

  let assems = stmt2assems stmt in

  (errors, assems, frags)

let show_frags ?ref_frags frags =
  let buffer = Buffer.create 10 in
  let rec go = function
    | [] -> ()
    | ((sym, frag) :: frags) ->
      begin
        Buffer.add_string buffer (Symbol.name sym);
        Buffer.add_string buffer ": ";
        Option.iter ref_frags
          ~f:(fun ref_frags ->
            match Map.find ref_frags sym with
            | None -> Buffer.add_string buffer "\t\t\t\t(diff)"
            | Some frag' ->
              if not (frag_eq frag frag') then
                Buffer.add_string buffer "\t\t\t\t(diff)");
        Buffer.add_char buffer '\n';
        Buffer.add_string buffer (show_frag frag);
        Buffer.add_string buffer "\n\n";
        go frags
      end
  in
    go frags;
    Buffer.contents buffer

let assert_ok (src : string) (expected_assems : string) (expected_frags : (Symbol.t * frag) list) =
  let (errors, assems, frags) = run_codegen src in

  if String.(errors <> "") then
    assert_failure
      ("errors:\n" ^ errors);

  let expected_assems = trim_assems expected_assems in
  let expected_frags = Map.of_alist_exn (module Symbol) expected_frags in

  if not (String.(expected_assems = assems) && Map.equal frag_eq frags expected_frags)
    then
      assert_failure
        (Printf.sprintf {|=========== expected ==========

Assembly:
%s

Fragments:
%s
========== actual ==========

Assembly:                         %s
%s

Fragments:
%s|}
         expected_assems
         (Map.to_alist expected_frags |> show_frags)
         (if String.(expected_assems = assems) then
            ""
          else
            "(diff)")
         assems
         (show_frags ~ref_frags:expected_frags (Map.to_alist frags)))

let test_codegen =
  let _L0 = Temp.named_label "_L0" in
  let _L1 = Temp.named_label "_L1" in
  let _L2 = Temp.named_label "_L2" in
  let _L3 = Temp.named_label "_L3" in
  let _L4 = Temp.named_label "_L4" in
  let _L5 = Temp.named_label "_L5" in
  let _L6 = Temp.named_label "_L6" in
  let _L7 = Temp.named_label "_L7" in

  "Test Codegen" >::: [

    "function call" >::: [

      "1 argument" >:: (fun _ ->
        assert_ok
          {|print("hello")|}
          {|
        _L2:
la t100, _L0
move $a0, t100
jal print
nop
j _L1
nop
        _L1:
|}
          [(_L0, S "hello")]);

      "4 arguments" >:: (fun _ ->
        assert_ok
          {|
let
  function foo(a:int,b:string,c:int,d:string): int = ord(b) + a * c
in
  foo(1,"hello",37+2,"world")
end
          |}
          {|
        _L6:
move $a0, $fp
ori t108, $0, 1
move $a1, t108
la t109, _L1
move $a2, t109
ori t110, $0, 39
move $a3, t110
la t111, _L2
addiu $sp, $sp, -4
sw t111, 0($sp)
jal _L0
nop
addiu $sp, $sp, 4
j _L5
nop
        _L5:
|}
          [(_L0, F {|
        _L4:
sw $a0, -4($fp)
nop
move t100, $a1
move t101, $a2
move t102, $a3
lw t105, 0($fp)
nop
move t103, t105
move $a0, t101
jal ord
nop
move t104, $v0
mult t100, t102
mflo t107
addu t106, t104, t107
move $v0, t106
j _L3
nop
        _L3:
|});
           (_L1, S "hello");
           (_L2, S "world")]);
    ];

    "arithmetic" >:: (fun _ ->
      assert_ok
        {|
let
  var a := 20
  var b := 37
in
  a := a * 3 - -b / 7 + 5 * -6
end
        |}
        {|
        _L1:
ori t100, $0, 20
ori t101, $0, 37
addiu t104, $0, -6
ori t105, $0, 5
mult t105, t104
mflo t103
ori t108, $0, 7
subu t109, $0, t101
divu t109, t108
mflo t107
ori t111, $0, 3
mult t100, t111
mflo t110
subu t106, t110, t107
addu t102, t106, t103
move t100, t102
j _L0
nop
        _L0:
        |}
        []);

    "sequence" >:: (fun _ ->
      assert_ok
        {|let var a := 20 in (1+2;((); nil; a := 27 + 6); print("abc"); 127) end|}
        {|
        _L2:
ori t100, $0, 20
ori t101, $0, 33
move t100, t101
la t102, _L0
move $a0, t102
jal print
nop
j _L1
nop
        _L1:
        |}
        [(_L0, S "abc")]);

    "array" >:: (fun _ ->
      assert_ok
        {|
let
  var a := 30
  type arr = array of int
  var b := arr[37 + 2] of a
in
  a := b[a*100]
end
        |}
        {|
        _L1:
ori t100, $0, 30
ori t102, $0, 39
move $a0, t102
move $a1, t100
jal alloc_array
nop
move t101, $v0
ori t106, $0, 4
ori t108, $0, 100
mult t100, t108
mflo t107
mult t107, t106
mflo t105
addu t104, t101, t105
lw t103, 0(t104)
nop
move t100, t103
j _L0
nop
        _L0:
|}
        []);

    "record" >:: (fun _ ->
      assert_ok
        {|
let
  type rec = { foo : int, bar : string, baz : rec }
  var a := rec { foo = 30, bar = "hello", baz = nil }
  var b := 20
in
  b := ord(a.bar) + a.foo * a.baz.foo
end
        |}
        {|
        _L2:
ori t104, $0, 12
move $a0, t104
jal alloc_record
nop
move t100, $v0
ori t105, $0, 30
sw t105, 0(t100)
nop
la t106, _L0
sw t106, 4(t100)
nop
sw $0, 8(t100)
nop
move t101, t100
ori t102, $0, 20
lw t107, 4(t101)
nop
move $a0, t107
jal ord
nop
move t103, $v0
lw t111, 8(t101)
nop
lw t110, 0(t111)
nop
lw t112, 0(t101)
nop
mult t112, t110
mflo t109
addu t108, t103, t109
move t102, t108
j _L1
nop
        _L1:
        |}
        [(_L0, S "hello")]);

    "assign" >::: [
      "variable" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 200
  var b := 300
  var c := "hey"
in
  c := concat(chr(a), chr(b));
  a := ord(c)
end
          |}
          {|
        _L2:
ori t100, $0, 200
ori t101, $0, 300
la t106, _L0
move t102, t106
move $a0, t100
jal chr
nop
move t103, $v0
move t105, t103
move $a0, t101
jal chr
nop
move t104, $v0
move $a0, t105
move $a1, t104
jal concat
nop
move t102, $v0
move $a0, t102
jal ord
nop
move t100, $v0
j _L1
nop
        _L1:
          |}
          [(_L0, S "hey")]);

      "array" >:: (fun _ ->
        assert_ok
          {|
let
  type arr = array of int
  var a := arr[0] of (30 * 7 + 2)
in
  a[-3] := 78
end
          |}
          {|
        _L1:
move $a0, $0
ori t102, $0, 210
addiu t101, t102, 2
move $a1, t101
jal alloc_array
nop
move t100, $v0
ori t105, $0, 4
addiu t106, $0, -3
mult t106, t105
mflo t104
addu t103, t100, t104
ori t107, $0, 78
sw t107, 0(t103)
nop
j _L0
nop
        _L0:
          |}
          []);

      "record" >:: (fun _ ->
        assert_ok
          {|
let
  type rec = { foo : string, bar : int }
  var a : rec := nil
in
  a.foo := "hello";
  a.bar := 37
end
          |}
          {|
        _L2:
ori t100, $0, 0
la t101, _L0
sw t101, 0(t100)
nop
ori t102, $0, 37
sw t102, 4(t100)
nop
j _L1
nop
        _L1:
          |}
          [(_L0, S "hello")]);

      "mixed" >:: (fun _ ->
        assert_ok
          {|
let
  type rec = { foo : arr, bar : int, baz : arr2 }
  type arr = array of rec
  type arr2 = array of int
  var a := arr[0] of nil
  var b : rec := nil
in
  a[27].baz[78] := 30;
  b.foo[b.bar].bar := 37
end
          |}
          {|
        _L1:
move $a0, $0
move $a1, $0
jal alloc_array
nop
move t100, $v0
ori t101, $0, 0
ori t103, $0, 312
ori t107, $0, 108
addu t106, t100, t107
lw t105, 0(t106)
nop
lw t104, 8(t105)
nop
addu t102, t104, t103
ori t108, $0, 30
sw t108, 0(t102)
nop
ori t112, $0, 4
lw t113, 4(t101)
nop
mult t113, t112
mflo t111
lw t114, 0(t101)
nop
addu t110, t114, t111
lw t109, 0(t110)
nop
ori t115, $0, 37
sw t115, 4(t109)
nop
j _L0
nop
        _L0:
          |}
          [])
    ];

    "if" >::: [
      "if-then-else" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
in
  if a > 2 then
    a := a + 1
  else
    a := 3 * a
end
          |}
          {|
        _L4:
ori t100, $0, 20
ori t102, $0, 2
slt t101, t102, t100
beq t101, $0, _L1
nop
        _L0:
addiu t103, t100, 1
move t100, t103
        _L2:
j _L3
nop
        _L1:
ori t105, $0, 3
mult t105, t100
mflo t104
move t100, t104
j _L2
nop
        _L3:
          |}
          []);

      "if-then" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  var b := 30
in
  if a >= b then
    a := b - 9
end
          |}
          {|
        _L4:
ori t100, $0, 20
ori t101, $0, 30
slt t102, t100, t101
bne t102, $0, _L1
nop
        _L0:
addiu t103, t101, -9
move t100, t103
        _L2:
j _L3
nop
        _L1:
j _L2
nop
        _L3:
          |}
          []);

      "mixed" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 27
  var b := a + 30
in
  if a = b + 2 then
    if 3 <> a then
      print("hello")
    else
      exit()
  else if a > b then
    a := b / 30
end
          |}
          {|
        _L11:
ori t100, $0, 27
addiu t102, t100, 30
move t101, t102
addiu t103, t101, 2
bne t100, t103, _L8
nop
        _L7:
ori t104, $0, 3
beq t104, t100, _L2
nop
        _L1:
la t105, _L0
move $a0, t105
jal print
nop
        _L3:
        _L9:
j _L10
nop
        _L2:
jal exit
nop
j _L3
nop
        _L8:
slt t106, t101, t100
beq t106, $0, _L5
nop
        _L4:
ori t108, $0, 30
divu t101, t108
mflo t107
move t100, t107
        _L6:
j _L9
nop
        _L5:
j _L6
nop
        _L10:
          |}
          [(_L0, S "hello")])
    ];

    "comparison" >::: [
      ">" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  var b := 30
in
  if a > b then
    a := a > 7
  else
    b := 13 > b
end
          |}
          {|
        _L8:
ori t100, $0, 20
ori t101, $0, 30
slt t104, t101, t100
beq t104, $0, _L5
nop
        _L4:
ori t102, $0, 1
ori t106, $0, 7
slt t105, t106, t100
beq t105, $0, _L1
nop
        _L0:
move t100, t102
        _L6:
j _L7
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L5:
ori t103, $0, 1
slti t107, t101, 13
beq t107, $0, _L3
nop
        _L2:
move t101, t103
j _L6
nop
        _L3:
ori t103, $0, 0
j _L2
nop
        _L7:
          |}
          []);

      "k1 > k2" >:: (fun _ ->
        assert_ok
          {|let var a := 3 > 2 var b := 2 > 3 in end|}
          {|
        _L5:
ori t101, $0, 1
        _L0:
move t100, t101
ori t103, $0, 1
j _L3
nop
        _L2:
move t102, t103
j _L4
nop
        _L1:
ori t101, $0, 0
j _L0
nop
        _L3:
ori t103, $0, 0
j _L2
nop
        _L4:
          |}
          []);

      "> with 0" >:: (fun _ ->
        assert_ok
          {|let var a := 1 var b := a > 0 var c := 0 > a in end|}
          {|
        _L5:
ori t100, $0, 1
ori t102, $0, 1
blez t100, _L1
nop
        _L0:
move t101, t102
ori t104, $0, 1
bgez t100, _L3
nop
        _L2:
move t103, t104
j _L4
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L3:
ori t104, $0, 0
j _L2
nop
        _L4:
          |}
          []);

      ">=" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  var b := 30
in
  if a >= b then
    a := a >= 7
  else
    b := 13 >= b
end
          |}
          {|
        _L8:
ori t100, $0, 20
ori t101, $0, 30
slt t104, t100, t101
bne t104, $0, _L5
nop
        _L4:
ori t102, $0, 1
slti t105, t100, 7
bne t105, $0, _L1
nop
        _L0:
move t100, t102
        _L6:
j _L7
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L5:
ori t103, $0, 1
ori t107, $0, 13
slt t106, t107, t101
bne t106, $0, _L3
nop
        _L2:
move t101, t103
j _L6
nop
        _L3:
ori t103, $0, 0
j _L2
nop
        _L7:
          |}
          []);

      ">= with 0" >:: (fun _ ->
        assert_ok
          {|let var a := 1 var b := a >= 0 var c := 0 >= a in end|}
          {|
        _L5:
ori t100, $0, 1
ori t102, $0, 1
bltz t100, _L1
nop
        _L0:
move t101, t102
ori t104, $0, 1
bgtz t100, _L3
nop
        _L2:
move t103, t104
j _L4
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L3:
ori t104, $0, 0
j _L2
nop
        _L4:
          |}
          []);

      "<" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  var b := 30
in
  if a < b then
    a := a < 7
  else
    b := 13 < b
end
          |}
          {|
        _L8:
ori t100, $0, 20
ori t101, $0, 30
slt t104, t100, t101
beq t104, $0, _L5
nop
        _L4:
ori t102, $0, 1
slti t105, t100, 7
beq t105, $0, _L1
nop
        _L0:
move t100, t102
        _L6:
j _L7
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L5:
ori t103, $0, 1
ori t107, $0, 13
slt t106, t107, t101
beq t106, $0, _L3
nop
        _L2:
move t101, t103
j _L6
nop
        _L3:
ori t103, $0, 0
j _L2
nop
        _L7:
          |}
          []);

      "< with 0" >:: (fun _ ->
        assert_ok
          {|let var a := 1 var b := a < 0 var c := 0 < a in end|}
          {|
        _L5:
ori t100, $0, 1
ori t102, $0, 1
bgez t100, _L1
nop
        _L0:
move t101, t102
ori t104, $0, 1
blez t100, _L3
nop
        _L2:
move t103, t104
j _L4
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L3:
ori t104, $0, 0
j _L2
nop
        _L4:
          |}
          []);

      "<=" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  var b := 30
in
  if a <= b then
    a := a <= 7
  else
    b := 13 <= b
end
          |}
          {|
        _L8:
ori t100, $0, 20
ori t101, $0, 30
slt t104, t101, t100
bne t104, $0, _L5
nop
        _L4:
ori t102, $0, 1
ori t106, $0, 7
slt t105, t106, t100
bne t105, $0, _L1
nop
        _L0:
move t100, t102
        _L6:
j _L7
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L5:
ori t103, $0, 1
slti t107, t101, 13
bne t107, $0, _L3
nop
        _L2:
move t101, t103
j _L6
nop
        _L3:
ori t103, $0, 0
j _L2
nop
        _L7:
          |}
          []);

      "<= with 0" >:: (fun _ ->
        assert_ok
          {|let var a := 1 var b := a <= 0 var c := 0 <= a in end|}
          {|
        _L5:
ori t100, $0, 1
ori t102, $0, 1
bgtz t100, _L1
nop
        _L0:
move t101, t102
ori t104, $0, 1
bltz t100, _L3
nop
        _L2:
move t103, t104
j _L4
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L3:
ori t104, $0, 0
j _L2
nop
        _L4:
          |}
          []);

      "=" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  var b := 30
in
  if a = b then
    a := a = 7
  else
    b := 13 = b
end
          |}
          {|
        _L8:
ori t100, $0, 20
ori t101, $0, 30
bne t100, t101, _L5
nop
        _L4:
ori t102, $0, 1
ori t104, $0, 7
bne t100, t104, _L1
nop
        _L0:
move t100, t102
        _L6:
j _L7
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L5:
ori t103, $0, 1
ori t105, $0, 13
bne t105, t101, _L3
nop
        _L2:
move t101, t103
j _L6
nop
        _L3:
ori t103, $0, 0
j _L2
nop
        _L7:
          |}
          []);

      "= with 0" >:: (fun _ ->
        assert_ok
          {|let var a := 1 var b := a = 0 var c := 0 = a in end|}
          {|
        _L5:
ori t100, $0, 1
ori t102, $0, 1
bne t100, $0, _L1
nop
        _L0:
move t101, t102
ori t104, $0, 1
bne $0, t100, _L3
nop
        _L2:
move t103, t104
j _L4
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L3:
ori t104, $0, 0
j _L2
nop
        _L4:
          |}
          []);

      "<>" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  var b := 30
in
  if a <> b then
    a := a <> 7
  else
    b := 13 <> b
end
          |}
          {|
        _L8:
ori t100, $0, 20
ori t101, $0, 30
beq t100, t101, _L5
nop
        _L4:
ori t102, $0, 1
ori t104, $0, 7
beq t100, t104, _L1
nop
        _L0:
move t100, t102
        _L6:
j _L7
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L5:
ori t103, $0, 1
ori t105, $0, 13
beq t105, t101, _L3
nop
        _L2:
move t101, t103
j _L6
nop
        _L3:
ori t103, $0, 0
j _L2
nop
        _L7:
          |}
          []);

      "<> with 0" >:: (fun _ ->
        assert_ok
          {|let var a := 1 var b := a <> 0 var c := 0 <> a in end|}
          {|
        _L5:
ori t100, $0, 1
ori t102, $0, 1
beq t100, $0, _L1
nop
        _L0:
move t101, t102
ori t104, $0, 1
beq $0, t100, _L3
nop
        _L2:
move t103, t104
j _L4
nop
        _L1:
ori t102, $0, 0
j _L0
nop
        _L3:
ori t104, $0, 0
j _L2
nop
        _L4:
          |}
          []);
    ];

    "logic and" >:: (fun _ ->
      assert_ok
        {|let var a := 46 var b := a > 89 & ord("hello") in end|}
        {|
        _L5:
ori t100, $0, 46
ori t102, $0, 1
ori t105, $0, 89
slt t104, t105, t100
beq t104, $0, _L3
nop
        _L1:
la t106, _L0
move $a0, t106
jal ord
nop
move t103, $v0
beq t103, $0, _L3
nop
        _L2:
move t101, t102
j _L4
nop
        _L3:
ori t102, $0, 0
j _L2
nop
        _L4:
        |}
        [(_L0, S "hello")]);

    "logic or" >:: (fun _ ->
      assert_ok
        {|let var a := 927 var b := 998 in a := 3785 <> a | a + b <= -3 end|}
        {|
        _L4:
ori t100, $0, 927
ori t101, $0, 998
ori t102, $0, 1
ori t103, $0, 3785
beq t103, t100, _L0
nop
        _L1:
move t100, t102
j _L3
nop
        _L0:
addu t105, t100, t101
addiu t106, $0, -3
slt t104, t106, t105
beq t104, $0, _L1
nop
        _L2:
ori t102, $0, 0
j _L1
nop
        _L3:
        |}
        []);

    "while loop" >:: (fun _ ->
      assert_ok
        {|
let
  var i := 127
in
  while i < 37264 do i := i + 1
end
        |}
        {|
        _L4:
ori t100, $0, 127
        _L1:
ori t102, $0, 37264
slt t101, t100, t102
beq t101, $0, _L0
nop
        _L2:
addiu t103, t100, 1
move t100, t103
j _L1
nop
        _L0:
j _L3
nop
        _L3:
        |}
        []);

    "for loop" >:: (fun _ ->
      assert_ok
        {|
let
  var a := 3128 - 2
in
  for i := -200 to a + 7 do
    print("hello")
end
        |}
        {|
        _L5:
ori t103, $0, 3126
move t100, t103
addiu t104, $0, -200
move t101, t104
addiu t105, t100, 7
move t102, t105
        _L3:
slt t106, t102, t101
bne t106, $0, _L0
nop
        _L2:
la t107, _L1
move $a0, t107
jal print
nop
addiu t108, t101, 1
move t101, t108
j _L3
nop
        _L0:
j _L4
nop
        _L4:
        |}
        [(_L1, S "hello")]);

    "break in while loop" >:: (fun _ ->
      assert_ok
        {|
let
  var a := 379
in
  while a > 12 do
    if a = 8 then
      break
end
        |}
        {|
        _L7:
ori t100, $0, 379
        _L4:
ori t102, $0, 12
slt t101, t102, t100
beq t101, $0, _L0
nop
        _L5:
ori t103, $0, 8
bne t100, t103, _L2
nop
        _L1:
        _L0:
j _L6
nop
        _L8:
        _L3:
j _L4
nop
        _L2:
j _L3
nop
        _L6:
        |}
        []);

    "break in for loop" >:: (fun _ ->
      assert_ok
        {|
let
  var a := 341
in
  for i := 304 to 14 do
    if a then
      break
    else
      a := a - 1
end
        |}
        {|
        _L7:
ori t100, $0, 341
ori t101, $0, 304
ori t102, $0, 14
        _L5:
slt t103, t102, t101
bne t103, $0, _L0
nop
        _L4:
beq t100, $0, _L2
nop
        _L1:
        _L0:
j _L6
nop
        _L8:
        _L3:
addiu t104, t101, 1
move t101, t104
j _L5
nop
        _L2:
addiu t105, t100, -1
move t100, t105
j _L3
nop
        _L6:
        |}
        [])
  ]

let () =
  run_test_tt_main test_codegen