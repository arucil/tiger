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

type frag =
  | F of string
  | S of string

let frag_eq x y =
  match x, y with
  | F asm1, F asm2 -> String.(asm1 = asm2)
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

  let expected_assems = String.strip ~drop:(fun c -> Char.(c = '\n')) expected_assems in
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
  let alloc_array = Symbol.sym "alloc_array" in
  let alloc_record = Symbol.sym "alloc_record" in
  let compare_str = Symbol.sym "compare_str" in
  let concat = Symbol.sym "concat" in
  let print = Symbol.sym "print" in
  let t2 = Temp.temp_of_int 2 in
  let t4 = Temp.temp_of_int 4 in
  let t5 = Temp.temp_of_int 5 in
  let t6 = Temp.temp_of_int 6 in
  let t7 = Temp.temp_of_int 7 in
  let t30 = Temp.temp_of_int 30 in
  let t100 = Temp.temp_of_int 100 in
  let t101 = Temp.temp_of_int 101 in
  let t102 = Temp.temp_of_int 102 in
  let t103 = Temp.temp_of_int 103 in
  let t104 = Temp.temp_of_int 104 in
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
or $a0, $0, t100
jal print
nop
j _L1
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
          {||}
          [(_L1, S "hello");
           (_L2, S "world")])
    ]
  ]

let () =
  run_test_tt_main test_codegen