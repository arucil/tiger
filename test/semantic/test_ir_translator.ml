open Base
open OUnit2
open Lex
open Parse
open Semantic
open Stdio

type frag =
  | F of Ir.stmt
  | S of string

let frag_eq x y =
  match x, y with
  | F stmt1, F stmt2 -> String.(Ir.show_stmt stmt1 = Ir.show_stmt stmt2)
  | S s1, S s2 -> String.(s1 = s2)
  | _ -> false

let show_frag = function
  | F stmt -> Ir.show_stmt stmt
  | S s -> Printf.sprintf {|"%s"|} s

let run_translator s =
  let temp_file = Caml.Filename.temp_file "tiger_semantic_" "" in
  let oc = Out_channel.create ~append:false temp_file in
  Errors.set_out oc;
  Errors.set_source_name "-";
  let expr = Parser.prog Lexer.get_token (Lexing.from_string s) in
  let trans = (module Translate.Make(Dummy_platf) : Translate.S with type fragment = Dummy_platf.fragment) in
  let (ty, stmt) = Analyzer.trans_prog (module (val trans) : Translate.S) expr in
  let module Trans = (val trans) in
  let frags = List.map (Trans.fragments ())
    ~f:(function
      | Fun (frame, stmt) ->
        (Dummy_platf.Frame.label frame, F stmt)
      | Str (label, str) ->
        (label, S str))
    |> Map.of_alist_exn (module Symbol)
  in
  Out_channel.close oc;
  let errors = In_channel.read_all temp_file in
  (ty, errors, stmt, frags)

let show_frags frags =
  let buffer = Buffer.create 10 in
  let rec go = function
    | [] -> ()
    | ((sym, frag) :: frags) ->
      begin
        Buffer.add_string buffer (Symbol.name sym);
        Buffer.add_string buffer ":\n";
        Buffer.add_string buffer (show_frag frag);
        Buffer.add_char buffer '\n';
        Buffer.add_char buffer '\n';
        go frags
      end
  in
    go frags;
    Buffer.contents buffer

let assert_ok (src : string) (expected_stmt : Ir.stmt) (expected_frags : (Symbol.t * frag) list) =
  let (_, actual_errors, stmt, frags) = run_translator src in
  let expected_frags = Map.of_alist_exn (module Symbol) expected_frags in
  if String.(actual_errors <> "") then
    assert_failure
      ("errors:\n" ^ actual_errors);

  if String.(Ir.show_stmt stmt = Ir.show_stmt expected_stmt) && Map.equal frag_eq frags expected_frags then
    ()
  else
    assert_failure
      (Printf.sprintf {|=========== expected ==========

Statements:
%s

Fragments:
%s
========== actual ==========

Statements:
%s

Fragments:
%s|}
         (Ir.show_stmt expected_stmt)
         (Map.to_alist expected_frags |> show_frags)
         (Ir.show_stmt stmt)
         (show_frags (Map.to_alist frags)))

let test_translator =
  let alloc_array = Symbol.sym "alloc_array" in
  let alloc_record = Symbol.sym "alloc_record" in
  let t100 = Temp.temp_of_int 100 in
  let _L0 = Temp.named_label "_L0" in
  let _L1 = Temp.named_label "_L1" in
  let _L2 = Temp.named_label "_L2" in
  let _L3 = Temp.named_label "_L3" in

  "Test Translator" >::: [

    "value" >::: [
      "nil" >:: (fun _ ->
        assert_ok
          {|nil|}
          (Ir.Expr (Ir.Const 0))
          []);

      "unit" >:: (fun _ ->
        assert_ok
          {|()|}
          (Ir.Expr (Ir.Const 0))
          []);

      "int" >:: (fun _ ->
        assert_ok
          {|1237|}
          (Ir.Expr (Ir.Const 1237))
          []);

      "string" >:: (fun _ ->
        assert_ok
          {|"abc-123"|}
          (Ir.Expr (Ir.Name _L0))
          [(_L0, S "abc-123")]);

      "array" >:: (fun _ ->
        assert_ok
          {|let type a = array of int in a[0] of 37 end|}
          (Ir.Expr (Ir.Call ((Ir.Name alloc_array), [(Ir.Const 0); (Ir.Const 37)])))
          []);

      "record" >:: (fun _ ->
        assert_ok
          {|let type a = {x:int} in a{x=20} end|}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Seq (
         (Ir.Move ((Ir.Temp t100),
            (Ir.Call ((Ir.Name alloc_record), [(Ir.Const 4)])))),
         (Ir.Move (
            (Ir.Mem (Ir.Binop (Ir.Add, (Ir.Temp t100), (Ir.Const 0)))),
            (Ir.Const 20)))
         )),
      (Ir.Temp t100))))
          []);
    ];

    "unary" >::: [
      "negation" >:: (fun _ ->
        assert_ok
          {|- 30|}
          (Ir.Expr (Ir.Unop (Ir.Neg, Ir.Const 30)))
          [])
    ];

    "binary" >::: [
      "arithmetic" >:: (fun _ ->
        assert_ok
          {|3+4-6*7/80|}
          (Ir.Expr
   (Ir.Binop (Ir.Sub, (Ir.Binop (Ir.Add, (Ir.Const 3), (Ir.Const 4))),
      (Ir.Binop (Ir.Div, (Ir.Binop (Ir.Mul, (Ir.Const 6), (Ir.Const 7))),
         (Ir.Const 80)))
      )))
          []);

      "comparison" >::: [
        "int" >:: (fun _ ->
          assert_ok
            {|3 > 2|}
            (Ir.Seq ((Ir.Cjump (Ir.Gt, (Ir.Const 3), (Ir.Const 2), _L0, _L0)),
   (Ir.Label _L0)))
            []);

        "string" >:: (fun _ ->
          assert_ok
            {|("what" <= "abc") + ("a" = "g")|}
            (Ir.Expr (Ir.Const 0))
            [(_L0, S "what");
             (_L1, S "abc");
             (_L2, S "a");
             (_L3, S "g")]);
      ]
    ]
  ]

let () =
  run_test_tt_main test_translator