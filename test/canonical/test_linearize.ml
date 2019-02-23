open Base
open OUnit2
open Lex
open Parse
open Semantic
open Canonical
open Stdio

let run_linearize s =
  let temp_file = Caml.Filename.temp_file "tiger_canonical_" "" in
  let oc = Out_channel.create ~append:false temp_file in
  Errors.set_out oc;
  Errors.set_source_name "-";
  let expr = Parser.prog Lexer.get_token (Lexing.from_string s) in
  let trans = (module Translate.Make(Dummy_platf) : Translate.S with type fragment = Dummy_platf.fragment) in
  let temp_store = Temp.new_store () in
  let (ty, stmt) = Analyzer.trans_prog expr (module (val trans) : Translate.S) temp_store in
  Out_channel.close oc;

  let module Trans = (val trans) in
  assert (List.for_all (Trans.fragments ())
    ~f:(function
      | Str _ -> true
      | _ -> false));

  let errors = In_channel.read_all temp_file in
  assert (String.is_empty errors);

  let stmts = Canon.linearize stmt temp_store in

  (ty, stmts)


let stmt_eq s1 s2 = String.(Ir.show_stmt s1 = Ir.show_stmt s2)

let show_stmts stmts =
  let rec go buf = function
    | [] -> Buffer.contents buf
    | s :: ss ->
      begin
        Buffer.add_string buf (Ir.show_stmt s);
        Buffer.add_char buf '\n';
        go buf ss;
      end
  in
      go (Buffer.create 10) stmts

let assert_ok (src : string) (expected_stmts : Ir.stmt list) =
  let (_, stmts) = run_linearize src in

  match List.for_all2 expected_stmts stmts ~f:stmt_eq with
  | Ok true -> ()
  | _ ->
    assert_failure
      (Printf.sprintf {|=========== expected ==========

Statements:
%s
========== actual ==========

Statements:
%s|}
      (show_stmts expected_stmts)
      (show_stmts stmts))

let test_linearize =
  "Test Linearize" >::: [
  ]

let () =
  run_test_tt_main test_linearize