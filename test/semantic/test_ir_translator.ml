open Base
open OUnit2
open Lex
open Parse
open Semantic
open Stdio
open Platform
open Common

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
  let trans = (module Translate.Make(Mips_platf) : Translate.S with type fragment = Mips_platf.fragment) in
  let temp_store = Temp.new_store () in
  let (ty, stmt) = Analyzer.trans_prog expr (module (val trans) : Translate.S) temp_store in
  let module Trans = (val trans) in
  let frags = List.map (Trans.fragments ())
    ~f:(function
      | Fun (frame, stmt) ->
        (Mips_platf.Frame.label frame, F stmt)
      | Str (label, str) ->
        (label, S str))
    |> Map.of_alist_exn (module Symbol)
  in
  Out_channel.close oc;
  let errors = In_channel.read_all temp_file in
  (ty, errors, stmt, frags)

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

Statements:                     %s
%s

Fragments:
%s|}
         (Ir.show_stmt expected_stmt)
         (Map.to_alist expected_frags |> show_frags)
         (if String.(Ir.show_stmt expected_stmt = Ir.show_stmt stmt) then
            ""
          else
            "(diff)")
         (Ir.show_stmt stmt)
         (show_frags ~ref_frags:expected_frags (Map.to_alist frags)))

let test_translator =
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

  "Test Translator" >::: [

    "value" >::: [
      "nil" >:: (fun _ ->
        assert_ok
          {|nil|}
          (Ir.Expr (Ir.Const 0l))
          []);

      "unit" >:: (fun _ ->
        assert_ok
          {|()|}
          (Ir.Expr (Ir.Const 0l))
          []);

      "int" >:: (fun _ ->
        assert_ok
          {|1237|}
          (Ir.Expr (Ir.Const 1237l))
          []);

      "string" >:: (fun _ ->
        assert_ok
          {|"abc-123"|}
          (Ir.Expr (Ir.Name _L0))
          [(_L0, S "abc-123")]);

      "array" >:: (fun _ ->
        assert_ok
          {|let type a = array of int in a[0] of 37 end|}
          (Ir.Expr (Ir.Call ((Ir.Name alloc_array), [(Ir.Const 0l); (Ir.Const 37l)])))
          []);

      "record" >:: (fun _ ->
        assert_ok
          {|let type a = {x:int} in a{x=20} end|}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Seq (
         (Ir.Move ((Ir.Temp t100),
            (Ir.Call ((Ir.Name alloc_record), [(Ir.Const 4l)])))),
         (Ir.Move (
            (Ir.Mem (Ir.Binop (Ir.Add, (Ir.Lval (Ir.Temp t100)), (Ir.Const 0l)))),
            (Ir.Const 20l)))
         )),
      (Ir.Lval (Ir.Temp t100)))))
          []);
    ];

    "unary" >::: [
      "negation" >:: (fun _ ->
        assert_ok
          {|- 30|}
          (Ir.Expr (Ir.Unop (Ir.Neg, Ir.Const 30l)))
          [])
    ];

    "binary" >::: [
      "arithmetic" >:: (fun _ ->
        assert_ok
          {|3+4-6*7/80|}
          (Ir.Expr
   (Ir.Binop (Ir.Sub, (Ir.Binop (Ir.Add, (Ir.Const 3l), (Ir.Const 4l))),
      (Ir.Binop (Ir.Div, (Ir.Binop (Ir.Mul, (Ir.Const 6l), (Ir.Const 7l))),
         (Ir.Const 80l)))
      )))
          []);

      "comparison" >::: [
        "int" >:: (fun _ ->
          assert_ok
            {|3 > 2|}
            (Ir.Seq ((Ir.Cjump (Ir.Gt, (Ir.Const 3l), (Ir.Const 2l), _L0, _L0)),
   (Ir.Label _L0)))
            []);

        "string" >:: (fun _ ->
          assert_ok
            {|("what" <= "abc") + ("a" = "g")|}
            (Ir.Expr
   (Ir.Binop (Ir.Add,
      (Ir.Eseq (
         (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 1l))),
            (Ir.Seq (
               (Ir.Cjump (Ir.Le,
                  (Ir.Call ((Ir.Name compare_str),
                     [(Ir.Name _L0); (Ir.Name _L1)])),
                  (Ir.Const 0l), _L6, _L7)),
               (Ir.Seq ((Ir.Label _L7),
                  (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 0l))),
                     (Ir.Label _L6)))
                  ))
               ))
            )),
         Ir.Lval (Ir.Temp t101))),
      (Ir.Eseq (
         (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 1l))),
            (Ir.Seq (
               (Ir.Cjump (Ir.Eq,
                  (Ir.Call ((Ir.Name compare_str),
                     [(Ir.Name _L2); (Ir.Name _L3)])),
                  (Ir.Const 0l), _L4, _L5)),
               (Ir.Seq ((Ir.Label _L5),
                  (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 0l))),
                     (Ir.Label _L4)))
                  ))
               ))
            )),
         Ir.Lval (Ir.Temp t100)))
      )))
            [(_L0, S "what");
             (_L1, S "abc");
             (_L2, S "a");
             (_L3, S "g")]);

        "array" >:: (fun _ ->
          assert_ok
            {|let type a = array of int var t := a[3] of 0 in t <> a[0] of 30 end|}
            (Ir.Expr
   (Ir.Eseq (
      (Ir.Move ((Ir.Temp t100),
         (Ir.Call ((Ir.Name alloc_array), [(Ir.Const 3l); (Ir.Const 0l)])))),
      (Ir.Eseq (
         (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 1l))),
            (Ir.Seq (
               (Ir.Cjump (Ir.Ne, Ir.Lval (Ir.Temp t100),
                  (Ir.Call ((Ir.Name alloc_array),
                     [(Ir.Const 0l); (Ir.Const 30l)])),
                  _L0, _L1)),
               (Ir.Seq ((Ir.Label _L1),
                  (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 0l))),
                     (Ir.Label _L0)))
                  ))
               ))
            )),
         Ir.Lval (Ir.Temp t101)))
      )))
            []);
      ];

    ];

    "init var and/or" >:: (fun _ ->
      assert_ok
        {|let var a := 3 & (2 <> 1) | -1 in end|}
        (Ir.Expr
   (Ir.Eseq (
      (Ir.Move ((Ir.Temp t100),
         (Ir.Eseq (
            (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 1l))),
               (Ir.Seq (
                  (Ir.Seq (
                     (Ir.Seq ((Ir.Jump ((Ir.Name _L0), [_L0])),
                        (Ir.Seq ((Ir.Label _L0),
                           (Ir.Cjump (Ir.Ne, (Ir.Const 2l), (Ir.Const 1l), _L2,
                              _L1))
                           ))
                        )),
                     (Ir.Seq ((Ir.Label _L1),
                        (Ir.Cjump (Ir.Ne, (Ir.Unop (Ir.Neg, (Ir.Const 1l))),
                           (Ir.Const 0l), _L2, _L3))
                        ))
                     )),
                  (Ir.Seq ((Ir.Label _L3),
                     (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 0l))),
                        (Ir.Label _L2)))
                     ))
                  ))
               )),
            Ir.Lval (Ir.Temp t101)))
         )),
      (Ir.Const 0l))))
        []);

    "and/or" >:: (fun _ ->
      assert_ok
        {|0 & (2 <> 1) | -1|}
        (Ir.Seq (
   (Ir.Seq (
      (Ir.Seq ((Ir.Jump ((Ir.Name _L1), [_L1])),
         (Ir.Seq ((Ir.Label _L0),
            (Ir.Cjump (Ir.Ne, (Ir.Const 2l), (Ir.Const 1l), _L2, _L1))))
         )),
      (Ir.Seq ((Ir.Label _L1),
         (Ir.Cjump (Ir.Ne, (Ir.Unop (Ir.Neg, (Ir.Const 1l))), (Ir.Const 0l),
            _L2, _L2))
         ))
      )),
   (Ir.Label _L2)))
        []);

    "variable" >:: (fun _ ->
      assert_ok
        {|let var a := 123 in a end|}
        (Ir.Expr (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 123l))), Ir.Lval (Ir.Temp t100))))
        []);

    "index" >:: (fun _ ->
      assert_ok
        {|let type a = array of int var t := a[0] of -1 in t[20] end|}
        (Ir.Expr
   (Ir.Eseq (
      (Ir.Move ((Ir.Temp t100),
         (Ir.Call ((Ir.Name alloc_array),
            [(Ir.Const 0l); (Ir.Unop (Ir.Neg, (Ir.Const 1l)))]))
         )),
      (Ir.Lval (Ir.Mem
         (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100),
            (Ir.Binop (Ir.Mul, (Ir.Const 20l), (Ir.Const 4l)))))))
      )))
        []);

    "field selection" >:: (fun _ ->
      assert_ok
        {|let type r1 = { bar:int,baz:string} var t : r1 := nil in t.baz end|}
        (Ir.Expr
   (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 0l))),
      (Ir.Lval (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 4l))))))))
        []);

    "index and field selection" >:: (fun _ ->
      assert_ok
        {|
let
  type a1 = array of int
  type r1 = { bar : a1 }
  type a2 = array of r1
  type r2 = { foo : a2 }
  var a : r2 := nil
in
  a.foo[2 + 3].bar
end
        |}
        (Ir.Expr
   (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 0l))),
      (Ir.Lval (Ir.Mem
         (Ir.Lval (Ir.Mem
            (Ir.Binop (Ir.Add, Ir.Lval (Ir.Mem (Ir.Lval (Ir.Temp t100))),
               (Ir.Binop (Ir.Mul,
                  (Ir.Binop (Ir.Add, (Ir.Const 2l), (Ir.Const 3l))),
                  (Ir.Const 4l)))
               ))))))
      )))
        []);

    "sequence" >:: (fun _ ->
      assert_ok
        {|
let
  var a := 20
  var b := "haha"
in
  (a := 29; b := ""; a + 2);
  (2+2;nil)
end
        |}
        (Ir.Expr
   (Ir.Eseq (
      (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 20l))),
         (Ir.Move ((Ir.Temp t101), (Ir.Name _L0))))),
      (Ir.Eseq (
         (Ir.Expr
            (Ir.Eseq (
               (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 29l))),
                  (Ir.Move ((Ir.Temp t101), (Ir.Name _L1))))),
               (Ir.Binop (Ir.Add, (Ir.Lval (Ir.Temp t100)), (Ir.Const 2l)))))),
         (Ir.Eseq ((Ir.Expr (Ir.Binop (Ir.Add, (Ir.Const 2l), (Ir.Const 2l)))),
            (Ir.Const 0l)))
         ))
      )))
        [(_L0, S "haha");
         (_L1, S "")]);

    "assignment" >::: [
      "int" >:: (fun _ ->
        assert_ok
          {|let var t := 20 in t := 27 end|}
          (Ir.Expr
   (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 20l))),
      (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 27l))), (Ir.Const 0l))))))
          []);

      "index" >:: (fun _ ->
        assert_ok
          {|let type a = array of int var t := a[0] of 0 in t[2] := 7 end|}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Move ((Ir.Temp t100),
         (Ir.Call ((Ir.Name alloc_array), [(Ir.Const 0l); (Ir.Const 0l)])))),
      (Ir.Eseq (
         (Ir.Move (
            (Ir.Mem
               (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100),
                  (Ir.Binop (Ir.Mul, (Ir.Const 2l), (Ir.Const 4l)))))),
            (Ir.Const 7l))),
         (Ir.Const 0l)))
      )))
          []);

      "field" >:: (fun _ ->
        assert_ok
          {|let type r = { foo:string, bar:int } var t : r := nil in t.bar := 20 end |}
          (Ir.Expr
   (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 0l))),
      (Ir.Eseq (
         (Ir.Move (
            (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 4l)))),
            (Ir.Const 20l))),
         (Ir.Const 0l)))
      )))
          []);
    ];

    "record creation" >:: (fun _ ->
      assert_ok
        {|
let
  type r = { a : int, b : string, c : int }
in
  r { a=20,b="abc",c=1007 }
end
        |}
        (Ir.Expr
   (Ir.Eseq (
      (Ir.Seq (
         (Ir.Move ((Ir.Temp t100),
            (Ir.Call ((Ir.Name alloc_record), [(Ir.Const 12l)])))),
         (Ir.Seq (
            (Ir.Move (
               (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 0l)))),
               (Ir.Const 20l))),
            (Ir.Seq (
               (Ir.Move (
                  (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 4l)))),
                  (Ir.Name _L0))),
               (Ir.Move (
                  (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 8l)))),
                  (Ir.Const 1007l)))
               )))))),
      Ir.Lval (Ir.Temp t100))))
        [(_L0, S "abc")]);

    "if" >::: [
      "if-then-else" >:: (fun _ ->
        assert_ok
          {|if 2 > 3 then 1 else 30|}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Seq ((Ir.Cjump (Ir.Gt, (Ir.Const 2l), (Ir.Const 3l), _L0, _L1)),
         (Ir.Seq ((Ir.Label _L0),
            (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 1l))),
               (Ir.Seq ((Ir.Jump ((Ir.Name _L2), [_L2])),
                  (Ir.Seq ((Ir.Label _L1),
                     (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 30l))),
                        (Ir.Label _L2)))
                     ))
                  ))
               ))
            ))
         )),
      Ir.Lval (Ir.Temp t100))))
          []);

      "if-then" >:: (fun _ ->
        assert_ok
          {|let var a := 3 in if a > 2 then a := 30 end|}
          (Ir.Expr
   (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 3l))),
      (Ir.Eseq (
         (Ir.Seq ((Ir.Cjump (Ir.Gt, Ir.Lval (Ir.Temp t100), (Ir.Const 2l), _L0, _L1)),
            (Ir.Seq ((Ir.Label _L0),
               (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 30l))),
                  (Ir.Seq ((Ir.Jump ((Ir.Name _L2), [_L2])),
                     (Ir.Seq ((Ir.Label _L1),
                        (Ir.Seq ((Ir.Expr (Ir.Const 0l)), (Ir.Label _L2)))))
                     ))
                  ))
               ))
            )),
         (Ir.Const 0l)))
      )))
          []);

      "if-int" >:: (fun _ ->
        assert_ok
          {|if 3 + 2 then 27 else 22*3|}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Seq (
         (Ir.Cjump (Ir.Ne, (Ir.Binop (Ir.Add, (Ir.Const 3l), (Ir.Const 2l))),
            (Ir.Const 0l), _L0, _L1)),
         (Ir.Seq ((Ir.Label _L0),
            (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 27l))),
               (Ir.Seq ((Ir.Jump ((Ir.Name _L2), [_L2])),
                  (Ir.Seq ((Ir.Label _L1),
                     (Ir.Seq (
                        (Ir.Move ((Ir.Temp t100),
                           (Ir.Binop (Ir.Mul, (Ir.Const 22l), (Ir.Const 3l))))),
                        (Ir.Label _L2))))))))))))),
      Ir.Lval (Ir.Temp t100))))
          []);
    ];

    "function call" >::: [

      "less than 4 params" >:: (fun _ ->
        assert_ok
          {|
let
  function foo(x : int, y : string): string = y
in
  concat("a", foo(3, "hy"))
end
          |}
          (Ir.Expr
   (Ir.Call ((Ir.Name concat),
      [(Ir.Name _L1);
        (Ir.Call ((Ir.Name _L0), [Ir.Lval (Ir.Temp t30); (Ir.Const 3l); (Ir.Name _L2)]
           ))])))
        [(_L0, F (Ir.Seq (
         (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
            Ir.Lval (Ir.Temp t4))),
         (Ir.Seq ((Ir.Move ((Ir.Temp t100), Ir.Lval (Ir.Temp t5))),
            (Ir.Seq ((Ir.Move ((Ir.Temp t101), Ir.Lval (Ir.Temp t6))),
               (Ir.Move ((Ir.Temp t2), Ir.Lval (Ir.Temp t101)))))
            ))
         )));
         (_L1, S "a");
         (_L2, S "hy")]);

      "more than 4 params" >:: (fun _ ->
        assert_ok
          {|
let
  function foo(x:int,y:int,z:int,w:string,u:int) = ()
in
  foo(1,2,3,"",4)
end
          |}
          (Ir.Expr
   (Ir.Call ((Ir.Name _L0),
      [Ir.Lval (Ir.Temp t30); (Ir.Const 1l); (Ir.Const 2l); (Ir.Const 3l);
        (Ir.Name _L1); (Ir.Const 4l)]
      )))
          [(_L0,
            F (Ir.Seq (
                (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
                   Ir.Lval (Ir.Temp t4))),
                (Ir.Seq ((Ir.Move ((Ir.Temp t100), Ir.Lval (Ir.Temp t5))),
                   (Ir.Seq ((Ir.Move ((Ir.Temp t101), Ir.Lval (Ir.Temp t6))),
                      (Ir.Seq ((Ir.Move ((Ir.Temp t102), Ir.Lval (Ir.Temp t7))),
                         (Ir.Seq (
                            (Ir.Move ((Ir.Temp t103),
                               (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const 0l))))),
                            (Ir.Seq (
                               (Ir.Move ((Ir.Temp t104),
                                  (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const 4l))))),
                               (Ir.Move ((Ir.Temp t2), (Ir.Const 0l)))))
                            ))
                         ))
                      ))
                   ))
                )));
           (_L1, S "")]);
    ];

    "while loop" >:: (fun _ ->
      assert_ok
        {|let var i := 2 in while i<10 do i := i+1 end|}
        (Ir.Expr
   (Ir.Eseq ((Ir.Move ((Ir.Temp t100), (Ir.Const 2l))),
      (Ir.Eseq (
         (Ir.Seq ((Ir.Label _L1),
            (Ir.Seq (
               (Ir.Cjump (Ir.Lt, Ir.Lval (Ir.Temp t100), (Ir.Const 10l), _L2, _L0)),
               (Ir.Seq ((Ir.Label _L2),
                  (Ir.Seq (
                     (Ir.Move ((Ir.Temp t100),
                        (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 1l))))),
                     (Ir.Seq ((Ir.Jump ((Ir.Name _L1), [_L1])),
                        (Ir.Label _L0)))
                     ))
                  ))
               ))
            )),
         (Ir.Const 0l)))
      )))
        []);

    "for loop" >:: (fun _ ->
      assert_ok
        {|for i := 1 to 10 do print("")|}
        (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 1l))),
   (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 10l))),
      (Ir.Seq ((Ir.Label _L3),
         (Ir.Seq (
            (Ir.Cjump (Ir.Le, Ir.Lval (Ir.Temp t100), Ir.Lval (Ir.Temp t101), _L2, _L0)),
            (Ir.Seq ((Ir.Label _L2),
               (Ir.Seq (
                  (Ir.Expr (Ir.Call ((Ir.Name print), [(Ir.Name _L1)]))),
                  (Ir.Seq (
                     (Ir.Move ((Ir.Temp t100),
                        (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 1l))))),
                     (Ir.Seq ((Ir.Jump ((Ir.Name _L3), [_L3])),
                        (Ir.Label _L0)))
                     ))
                  ))
               ))
            ))
         ))
      ))
   ))
        [(_L1, S "")]);

    "break in while loop" >:: (fun _ ->
      assert_ok
        {|while 1 do if 3 > 2 then break|}
        (Ir.Seq ((Ir.Label _L4),
   (Ir.Seq ((Ir.Jump ((Ir.Name _L5), [_L5])),
      (Ir.Seq ((Ir.Label _L5),
         (Ir.Seq (
            (Ir.Seq (
               (Ir.Cjump (Ir.Gt, (Ir.Const 3l), (Ir.Const 2l), _L1, _L2)),
               (Ir.Seq ((Ir.Label _L1),
                  (Ir.Seq ((Ir.Jump ((Ir.Name _L0), [_L0])),
                     (Ir.Seq ((Ir.Jump ((Ir.Name _L3), [_L3])),
                        (Ir.Seq ((Ir.Label _L2),
                           (Ir.Seq ((Ir.Expr (Ir.Const 0l)), (Ir.Label _L3)))
                           ))
                        ))
                     ))
                  ))
               )),
            (Ir.Seq ((Ir.Jump ((Ir.Name _L4), [_L4])), (Ir.Label _L0)))))
         ))
      ))
   ))
        []);

    "break in for loop" >:: (fun _ ->
      assert_ok
        {|for i := 1 to 5 do if i = 2 then break|}
        (Ir.Seq ((Ir.Move ((Ir.Temp t100), (Ir.Const 1l))),
   (Ir.Seq ((Ir.Move ((Ir.Temp t101), (Ir.Const 5l))),
      (Ir.Seq ((Ir.Label _L5),
         (Ir.Seq (
            (Ir.Cjump (Ir.Le, Ir.Lval (Ir.Temp t100), Ir.Lval (Ir.Temp t101), _L4, _L0)),
            (Ir.Seq ((Ir.Label _L4),
               (Ir.Seq (
                  (Ir.Seq (
                     (Ir.Cjump (Ir.Eq, Ir.Lval (Ir.Temp t100), (Ir.Const 2l), _L1, _L2
                        )),
                     (Ir.Seq ((Ir.Label _L1),
                        (Ir.Seq ((Ir.Jump ((Ir.Name _L0), [_L0])),
                           (Ir.Seq ((Ir.Jump ((Ir.Name _L3), [_L3])),
                              (Ir.Seq ((Ir.Label _L2),
                                 (Ir.Seq ((Ir.Expr (Ir.Const 0l)),
                                    (Ir.Label _L3)))
                                 ))
                              ))
                           ))
                        ))
                     )),
                  (Ir.Seq (
                     (Ir.Move ((Ir.Temp t100),
                        (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t100), (Ir.Const 1l))))),
                     (Ir.Seq ((Ir.Jump ((Ir.Name _L5), [_L5])),
                        (Ir.Label _L0)))
                     ))
                  ))
               ))
            ))
         ))
      ))
   ))
        []);

    "static link" >::: [
      "1 level" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
  function foo(x:int,y:int):int = a + x
in
  foo(1,2)
end
          |}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
         (Ir.Const 20l))),
      (Ir.Call ((Ir.Name _L0), [Ir.Lval (Ir.Temp t30); (Ir.Const 1l); (Ir.Const 2l)]))
      )))
          [(_L0, F (Ir.Seq (
            (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
               Ir.Lval (Ir.Temp t4))),
            (Ir.Seq ((Ir.Move ((Ir.Temp t100), Ir.Lval (Ir.Temp t5))),
               (Ir.Seq ((Ir.Move ((Ir.Temp t101), Ir.Lval (Ir.Temp t6))),
                  (Ir.Move ((Ir.Temp t2),
                     (Ir.Binop (Ir.Add,
                        (Ir.Lval (Ir.Mem
                           (Ir.Binop (Ir.Add,
                              (Ir.Lval (Ir.Mem
                                 (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l)))))),
                              (Ir.Const (-4l)))))),
                        Ir.Lval (Ir.Temp t100)))
                     ))
                  ))
               ))
            )))]);

      "many levels" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 37
  function foo(x:int,y:int):int =
    let
      function bar(x:int):int = a-x
    in
      foo(1,2)+bar(3)
    end
in
end
          |}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
         (Ir.Const 37l))),
      (Ir.Const 0l))))
          [(_L0, F (Ir.Seq (
            (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
               Ir.Lval (Ir.Temp t4))),
            (Ir.Seq ((Ir.Move ((Ir.Temp t100), Ir.Lval (Ir.Temp t5))),
               (Ir.Seq ((Ir.Move ((Ir.Temp t101), Ir.Lval (Ir.Temp t6))),
                  (Ir.Move ((Ir.Temp t2),
                     (Ir.Binop (Ir.Add,
                        (Ir.Call ((Ir.Name _L0),
                           [Ir.Lval (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l)))));
                             (Ir.Const 1l); (Ir.Const 2l)]
                           )),
                        (Ir.Call ((Ir.Name _L1), [Ir.Lval (Ir.Temp t30); (Ir.Const 3l)]))))
                     ))
                  ))
               ))
            )));
           (_L1, F (Ir.Seq (
            (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
               Ir.Lval (Ir.Temp t4))),
            (Ir.Seq ((Ir.Move ((Ir.Temp t102), Ir.Lval (Ir.Temp t5))),
               (Ir.Move ((Ir.Temp t2),
                  (Ir.Binop (Ir.Sub,
                     (Ir.Lval (Ir.Mem
                        (Ir.Binop (Ir.Add,
                           (Ir.Lval (Ir.Mem
                              (Ir.Binop (Ir.Add,
                                 (Ir.Lval (Ir.Mem
                                    (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l)))))),
                                 (Ir.Const (-4l)))))),
                           (Ir.Const (-4l)))))),
                     Ir.Lval (Ir.Temp t102)))
                  ))
               ))
            )))]);

      "for var escape" >:: (fun _ ->
        assert_ok
          {|
let
  var a := 20
in
  for i := 1 to a do
    let
      function f(j:int) : int = a + i
    in
      f(i);
      ()
    end
end
          |}
          (Ir.Expr
   (Ir.Eseq (
      (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
         (Ir.Const 20l))),
      (Ir.Eseq (
         (Ir.Seq (
            (Ir.Move (
               (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-8l))))),
               (Ir.Const 1l))),
            (Ir.Seq (
               (Ir.Move ((Ir.Temp t101),
                  (Ir.Lval (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))))
                  )),
               (Ir.Seq ((Ir.Label _L3),
                  (Ir.Seq (
                     (Ir.Cjump (Ir.Le,
                        (Ir.Lval (Ir.Mem
                           (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-8l)))))),
                        Ir.Lval (Ir.Temp t101), _L2, _L0)),
                     (Ir.Seq ((Ir.Label _L2),
                        (Ir.Seq (
                           (Ir.Expr
                              (Ir.Eseq (
                                 (Ir.Expr
                                    (Ir.Call ((Ir.Name _L1),
                                       [Ir.Lval (Ir.Temp t30);
                                         Ir.Lval (Ir.Mem
                                            (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30),
                                               (Ir.Const (-8l)))))
                                         ]
                                       ))),
                                 (Ir.Const 0l)))),
                           (Ir.Seq (
                              (Ir.Move (
                                 (Ir.Mem
                                    (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30),
                                       (Ir.Const (-8l))))),
                                 (Ir.Binop (Ir.Add,
                                    (Ir.Lval (Ir.Mem
                                       (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30),
                                          (Ir.Const (-8l)))))),
                                    (Ir.Const 1l)))
                                 )),
                              (Ir.Seq ((Ir.Jump ((Ir.Name _L3), [_L3])),
                                 (Ir.Label _L0)))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            )),
         (Ir.Const 0l)))
      )))
          [(_L1, F (Ir.Seq (
            (Ir.Move ((Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l))))),
               Ir.Lval (Ir.Temp t4))),
            (Ir.Seq ((Ir.Move ((Ir.Temp t100), Ir.Lval (Ir.Temp t5))),
               (Ir.Move ((Ir.Temp t2),
                  (Ir.Binop (Ir.Add,
                     (Ir.Lval (Ir.Mem
                        (Ir.Binop (Ir.Add,
                           (Ir.Lval (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l)))))),
                           (Ir.Const (-4l)))))),
                     (Ir.Lval (Ir.Mem
                        (Ir.Binop (Ir.Add,
                           (Ir.Lval (Ir.Mem (Ir.Binop (Ir.Add, Ir.Lval (Ir.Temp t30), (Ir.Const (-4l)))))),
                           (Ir.Const (-8l))))))
                     ))
                  ))
               ))
            ))
            )]);
    ];
  ]

let () =
  run_test_tt_main test_translator