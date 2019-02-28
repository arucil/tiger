open Base
open OUnit2
open Lex
open Parse
open Semantic
open Canonical
open Stdio
open Platform
open Common

let run_trace_schedule s =
  let temp_file = Caml.Filename.temp_file "tiger_canonical_" "" in
  let oc = Out_channel.create ~append:false temp_file in
  Errors.set_out oc;
  Errors.set_source_name "-";
  let expr = Parser.prog Lexer.get_token (Lexing.from_string s) in
  let trans = (module Translate.Make(Mips_platf) : Translate.S with type fragment = Mips_platf.fragment) in
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
  let (blocks, done_label) = Canon.basic_blocks stmts temp_store in
  let stmts = Canon.trace_schedule blocks done_label temp_store in

  (ty, stmts)


let stmt_eq s1 s2 = String.(Ir.show_stmt s1 = Ir.show_stmt s2)

let show_stmts stmts =
  let buf = Buffer.create 10 in
  let rec go = function
    | [] -> ()
    | s :: ss ->
      begin
        Buffer.add_string buf (Ir.show_stmt s);
        Buffer.add_string buf ";\n";
        go ss;
      end
  in
  Buffer.add_string buf "[\n";
  go stmts;
  Buffer.add_string buf "]\n";
  Buffer.contents buf

let assert_ok (src : string) (expected_stmts : Ir.stmt list) =
  let (_, stmts) = run_trace_schedule src in

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

let test_trace =
  let print = Symbol.sym "print" in
  let ord = Symbol.sym "ord" in
  let t100 = Temp.temp_of_int 100 in
  let t101 = Temp.temp_of_int 101 in
  let t102 = Temp.temp_of_int 102 in
  let _L0 = Temp.named_label "_L0" in
  let _L1 = Temp.named_label "_L1" in
  let _L2 = Temp.named_label "_L2" in
  let _L3 = Temp.named_label "_L3" in
  let _L4 = Temp.named_label "_L4" in
  let _L5 = Temp.named_label "_L5" in
  let _L6 = Temp.named_label "_L6" in
  let _L7 = Temp.named_label "_L7" in
  let _L8 = Temp.named_label "_L8" in

  "Test Trace" >::: [

    "if" >:: (fun _ ->
      assert_ok
        {|if 3 = 0 then print("a") else print("b")|}
        [
(Ir.Label _L6);
(Ir.Cjump (Ir.Ne, (Ir.Const 3), (Ir.Const 0), _L3, _L2));
(Ir.Label _L2);
(Ir.Expr (Ir.Call ((Ir.Name print), [(Ir.Name _L0)])));
(Ir.Label _L4);
(Ir.Jump ((Ir.Name _L5), [_L5]));
(Ir.Label _L3);
(Ir.Expr (Ir.Call ((Ir.Name print), [(Ir.Name _L1)])));
(Ir.Jump ((Ir.Name _L4), [_L4]));
(Ir.Label _L5);
]
        );

    "while loop" >:: (fun _ ->
      assert_ok
        {|let var a := 2 in while a < 10 do a := a + 1 end|}
        [
(Ir.Label _L4);
(Ir.Move ((Ir.Temp t100), (Ir.Const 2)));
(Ir.Label _L1);
(Ir.Cjump (Ir.Ge, (Ir.Lval (Ir.Temp t100)), (Ir.Const 10), _L0, _L2));
(Ir.Label _L2);
(Ir.Move ((Ir.Temp t100),
   (Ir.Binop (Ir.Add, (Ir.Lval (Ir.Temp t100)), (Ir.Const 1)))));
(Ir.Jump ((Ir.Name _L1), [_L1]));
(Ir.Label _L0);
(Ir.Jump ((Ir.Name _L3), [_L3]));
(Ir.Label _L3);
]
        );

    "break" >:: (fun _ ->
      assert_ok
        {|
let
  var a := "a"
  var i := 20
in
  while ord(a) do
    if i < 10 then
      i := i + 1
    else
      (print(a); break)
end|}
[
(Ir.Label _L8);
(Ir.Move ((Ir.Temp t100), (Ir.Name _L0)));
(Ir.Move ((Ir.Temp t101), (Ir.Const 20)));
(Ir.Label _L5);
(Ir.Move ((Ir.Temp t102),
   (Ir.Call ((Ir.Name ord), [(Ir.Lval (Ir.Temp t100))]))));
(Ir.Cjump (Ir.Eq, (Ir.Lval (Ir.Temp t102)), (Ir.Const 0), _L1, _L6));
(Ir.Label _L6);
(Ir.Cjump (Ir.Ge, (Ir.Lval (Ir.Temp t101)), (Ir.Const 10), _L3, _L2));
(Ir.Label _L2);
(Ir.Move ((Ir.Temp t101),
   (Ir.Binop (Ir.Add, (Ir.Lval (Ir.Temp t101)), (Ir.Const 1)))));
(Ir.Label _L4);
(Ir.Jump ((Ir.Name _L5), [_L5]));
(Ir.Label _L3);
(Ir.Expr (Ir.Call ((Ir.Name print), [(Ir.Lval (Ir.Temp t100))])));
(Ir.Label _L1);
(Ir.Jump ((Ir.Name _L7), [_L7]));
(Ir.Label _L7);
]
        );
  ]

let () =
  run_test_tt_main test_trace