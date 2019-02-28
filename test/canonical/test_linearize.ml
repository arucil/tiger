open Base
open OUnit2
open Lex
open Parse
open Semantic
open Canonical
open Stdio
open Platform
open Common

let run_linearize s =
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
  let print = Symbol.sym "print" in
  let chr = Symbol.sym "chr" in
  let ord = Symbol.sym "ord" in
  let alloc_array = Symbol.sym "alloc_array" in
  let getchar = Symbol.sym "getchar" in
  let t100 = Temp.temp_of_int 100 in
  let t101 = Temp.temp_of_int 101 in
  let t102 = Temp.temp_of_int 102 in
  let t103 = Temp.temp_of_int 103 in
  let _L0 = Temp.named_label "_L0" in
  let _L1 = Temp.named_label "_L1" in

  "Test Linearize" >::: [

    "flatten Seq" >:: (fun _ ->
      assert_ok
        {|let var a := 2 in a := a + 1; print("h"); a end|}
        [
(Ir.Move ((Ir.Temp t100), (Ir.Const 2)));
(Ir.Move ((Ir.Temp t100),
   (Ir.Binop (Ir.Add, (Ir.Lval (Ir.Temp t100)), (Ir.Const 1)))));
(Ir.Expr (Ir.Call ((Ir.Name print), [(Ir.Name _L0)])));
(Ir.Expr (Ir.Lval (Ir.Temp t100)));
]
        );

    "lift stmt in binary" >:: (fun _ ->
      assert_ok
        {|let var a := 2 in (a := 3; a - 1) + (print("abc"); 307; 2 * 4) end|}
        [
(Ir.Move ((Ir.Temp t100), (Ir.Const 2)));
(Ir.Move ((Ir.Temp t100), (Ir.Const 3)));
(Ir.Move ((Ir.Temp t101),
   (Ir.Binop (Ir.Sub, (Ir.Lval (Ir.Temp t100)), (Ir.Const 1)))));
(Ir.Expr (Ir.Call ((Ir.Name print), [(Ir.Name _L0)])));
(Ir.Expr
   (Ir.Binop (Ir.Add, (Ir.Lval (Ir.Temp t101)),
      (Ir.Binop (Ir.Mul, (Ir.Const 2), (Ir.Const 4))))));
]
        );

    "lift stmt in binary w/o temp" >:: (fun _ ->
      assert_ok
        {|let var a := 2 in (a := 3; 7) + (print("abc"); 307; 2 * 4) end|}
        [
(Ir.Move ((Ir.Temp t100), (Ir.Const 2)));
(Ir.Move ((Ir.Temp t100), (Ir.Const 3)));
(Ir.Expr (Ir.Call ((Ir.Name print), [(Ir.Name _L0)])));
(Ir.Expr
   (Ir.Binop (Ir.Add, (Ir.Const 7),
      (Ir.Binop (Ir.Mul, (Ir.Const 2), (Ir.Const 4))))));
]
        );

    "flatten function call" >:: (fun _ ->
      assert_ok
        {|let var a := () in a := print(chr(3 + ord("a"))) end|}
        [
(Ir.Move ((Ir.Temp t100), (Ir.Const 0)));
(Ir.Move ((Ir.Temp t102), (Ir.Call ((Ir.Name ord), [(Ir.Name _L0)]))));
(Ir.Move ((Ir.Temp t101),
   (Ir.Call ((Ir.Name chr),
      [(Ir.Binop (Ir.Add, (Ir.Const 3), (Ir.Lval (Ir.Temp t102))))]))
   ));
(Ir.Move ((Ir.Temp t100),
   (Ir.Call ((Ir.Name print), [(Ir.Lval (Ir.Temp t101))]))));
]
        );

    "lift stmt in index" >:: (fun _ ->
      assert_ok
        {|
let
  type a = array of int
  var a := a[1] of ord("a")
in
  a[(getchar(); ord("012")*3)] := 37
end
        |}
        [
(Ir.Move ((Ir.Temp t101), (Ir.Call ((Ir.Name ord), [(Ir.Name _L0)]))));
(Ir.Move ((Ir.Temp t100),
   (Ir.Call ((Ir.Name alloc_array), [(Ir.Const 1); (Ir.Lval (Ir.Temp t101))]
      ))
   ));
(Ir.Move ((Ir.Temp t103), (Ir.Lval (Ir.Temp t100))));
(Ir.Expr (Ir.Call ((Ir.Name getchar), [])));
(Ir.Move ((Ir.Temp t102), (Ir.Call ((Ir.Name ord), [(Ir.Name _L1)]))));
(Ir.Move (
   (Ir.Mem
      (Ir.Binop (Ir.Add, (Ir.Lval (Ir.Temp t103)),
         (Ir.Binop (Ir.Mul,
            (Ir.Binop (Ir.Mul, (Ir.Lval (Ir.Temp t102)), (Ir.Const 3))),
            (Ir.Const 4)))
         ))),
   (Ir.Const 37)));
]
        )
  ]

let () =
  run_test_tt_main test_linearize