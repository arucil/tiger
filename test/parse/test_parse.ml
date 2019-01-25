open Base
open OUnit2
open Parse
open Lex
open Ast

let run_parser s =
  Parser.prog Lexer.get_token (Lexing.from_string s)

let assert_ok (src : string) (expected : expr) =
  let actual = run_parser src in
  if Poly.(actual = expected) then
    ()
  else
    assert_failure
      (Printf.sprintf {|=========== expected ==========

%s

========== actual ==========

%s
|}
         (show_expr expected)
         (show_expr actual))

let test_parse =
  "Test Parser" >::: [


    "arithmetic" >:: (fun _ ->
      assert_ok
        {|1 + (2 - 3 * 4) / 5 - 6 + -7 |}
        ((1, 25),
        Ast.BinaryExpr {
          lhs =
          ((1, 21),
           Ast.BinaryExpr {
             lhs =
             ((1, 3),
              Ast.BinaryExpr {lhs = ((1, 1), (Ast.IntExpr 1)); op = Ast.AddOp;
                rhs =
                ((1, 17),
                 Ast.BinaryExpr {
                   lhs =
                   ((1, 8),
                    Ast.BinaryExpr {lhs = ((1, 6), (Ast.IntExpr 2)); op = Ast.SubOp;
                      rhs =
                      ((1, 12),
                       Ast.BinaryExpr {lhs = ((1, 10), (Ast.IntExpr 3));
                         op = Ast.MulOp; rhs = ((1, 14), (Ast.IntExpr 4))})});
                   op = Ast.DivOp; rhs = ((1, 19), (Ast.IntExpr 5))})});
             op = Ast.SubOp; rhs = ((1, 23), (Ast.IntExpr 6))});
          op = Ast.AddOp;
          rhs =
          ((1, 27),
           Ast.UnaryExpr {op = Ast.NegOp; rand = ((1, 28), (Ast.IntExpr 7))})}));


    "primitive" >:: (fun _ ->
      assert_ok
        {|123*"he\ty" - ()|}
        ((1, 13),
        Ast.BinaryExpr {
          lhs =
          ((1, 4),
           Ast.BinaryExpr {lhs = ((1, 1), (Ast.IntExpr 123)); op = Ast.MulOp;
             rhs = ((1, 5), (Ast.StrExpr "he\ty"))});
          op = Ast.SubOp; rhs = ((1, 15), Ast.NilExpr)}));


    "sequence" >:: (fun _ ->
      assert_ok
        {|(1+2)+(1*3;2-4)/(1;2;3;4)|}
        ((1, 6),
        Ast.BinaryExpr {
          lhs =
          ((1, 3),
            Ast.BinaryExpr {lhs = ((1, 2), (Ast.IntExpr 1)); op = Ast.AddOp;
              rhs = ((1, 4), (Ast.IntExpr 2))});
          op = Ast.AddOp;
          rhs =
          ((1, 16),
            Ast.BinaryExpr {
              lhs =
              ((1, 7),
              (Ast.SeqExpr
                  [((1, 9),
                    Ast.BinaryExpr {lhs = ((1, 8), (Ast.IntExpr 1)); op = Ast.MulOp;
                      rhs = ((1, 10), (Ast.IntExpr 3))});
                    ((1, 13),
                    Ast.BinaryExpr {lhs = ((1, 12), (Ast.IntExpr 2));
                      op = Ast.SubOp; rhs = ((1, 14), (Ast.IntExpr 4))})
                    ]));
              op = Ast.DivOp;
              rhs =
              ((1, 17),
              (Ast.SeqExpr
                  [((1, 18), (Ast.IntExpr 1)); ((1, 20), (Ast.IntExpr 2));
                    ((1, 22), (Ast.IntExpr 3)); ((1, 24), (Ast.IntExpr 4))]))})}));


    "comparison" >:: (fun _ ->
      assert_ok
        {|(2 = (2 <> (3 < 7; 10 <= 11; 78 >= 12))) > "foo"|}
        ((1, 42),
        Ast.BinaryExpr {
          lhs =
          ((1, 4),
            Ast.BinaryExpr {lhs = ((1, 2), (Ast.IntExpr 2)); op = Ast.EqOp;
              rhs =
              ((1, 9),
              Ast.BinaryExpr {lhs = ((1, 7), (Ast.IntExpr 2)); op = Ast.NeqOp;
                rhs =
                ((1, 12),
                  (Ast.SeqExpr
                    [((1, 15),
                      Ast.BinaryExpr {lhs = ((1, 13), (Ast.IntExpr 3));
                        op = Ast.LtOp; rhs = ((1, 17), (Ast.IntExpr 7))});
                      ((1, 23),
                        Ast.BinaryExpr {lhs = ((1, 20), (Ast.IntExpr 10));
                          op = Ast.LeOp; rhs = ((1, 26), (Ast.IntExpr 11))});
                      ((1, 33),
                        Ast.BinaryExpr {lhs = ((1, 30), (Ast.IntExpr 78));
                          op = Ast.GeOp; rhs = ((1, 36), (Ast.IntExpr 12))})
                      ]))})});
          op = Ast.GtOp; rhs = ((1, 44), (Ast.StrExpr "foo"))}));


    "logical" >:: (fun _ ->
      assert_ok
        {|0 | 3 = 3 & () & 99 | 2 > 1|}
        ((1, 21),
        Ast.BinaryExpr {
          lhs =
          ((1, 3),
            Ast.BinaryExpr {lhs = ((1, 1), (Ast.IntExpr 0)); op = Ast.OrOp;
              rhs =
              ((1, 16),
              Ast.BinaryExpr {
                lhs =
                ((1, 11),
                  Ast.BinaryExpr {
                    lhs =
                    ((1, 7),
                    Ast.BinaryExpr {lhs = ((1, 5), (Ast.IntExpr 3)); op = Ast.EqOp;
                      rhs = ((1, 9), (Ast.IntExpr 3))});
                    op = Ast.AndOp; rhs = ((1, 13), Ast.NilExpr)});
                op = Ast.AndOp; rhs = ((1, 18), (Ast.IntExpr 99))})});
          op = Ast.OrOp;
          rhs =
          ((1, 25),
            Ast.BinaryExpr {lhs = ((1, 23), (Ast.IntExpr 2)); op = Ast.GtOp;
              rhs = ((1, 27), (Ast.IntExpr 1))})}));


    "call" >:: (fun _ ->
      assert_ok
        {|foo()+bar(baz(7+2),-3,"\"hey\"")|}
        ((1, 6),
        Ast.BinaryExpr {lhs = ((1, 1), Ast.CallExpr {func = "foo"; args = []});
          op = Ast.AddOp;
          rhs =
          ((1, 7),
            Ast.CallExpr {func = "bar";
              args =
              [((1, 11),
                Ast.CallExpr {func = "baz";
                  args =
                  [((1, 16),
                    Ast.BinaryExpr {lhs = ((1, 15), (Ast.IntExpr 7)); op = Ast.AddOp;
                      rhs = ((1, 17), (Ast.IntExpr 2))})
                    ]});
                ((1, 20),
                Ast.UnaryExpr {op = Ast.NegOp; rand = ((1, 21), (Ast.IntExpr 3))});
                ((1, 23), (Ast.StrExpr "\"hey\""))]})}));


    "lvalue" >:: (fun _ ->
      assert_ok
        {|foo.bar + baz[-abc].qux.Hoge|}
        ((1, 9),
        Ast.BinaryExpr {
          lhs =
          ((1, 5),
            (Ast.VarExpr
              (Ast.FieldVar ((1, 5), (Ast.SimpleVar ((1, 1), "foo")), "bar"))));
          op = Ast.AddOp;
          rhs =
          ((1, 25),
            (Ast.VarExpr
              (Ast.FieldVar ((1, 25),
                  (Ast.FieldVar ((1, 21),
                    (Ast.IndexVar ((1, 14), (Ast.SimpleVar ((1, 11), "baz")),
                        ((1, 15),
                        Ast.UnaryExpr {op = Ast.NegOp;
                          rand =
                          ((1, 16), (Ast.VarExpr (Ast.SimpleVar ((1, 16), "abc"))))})
                        )),
                    "qux")),
                  "Hoge"))))}));


    "assignment" >:: (fun _ ->
      assert_ok
        {|(a := (bB[27] := "qaz"); foo.bar[baz] := ())|}
        ((1, 1),
        (Ast.SeqExpr
            [((1, 4),
              Ast.AssignExpr {var = (Ast.SimpleVar ((1, 2), "a"));
                expr =
                ((1, 15),
                Ast.AssignExpr {
                  var =
                  (Ast.IndexVar ((1, 10), (Ast.SimpleVar ((1, 8), "bB")),
                      ((1, 11), (Ast.IntExpr 27))));
                  expr = ((1, 18), (Ast.StrExpr "qaz"))})});
              ((1, 39),
              Ast.AssignExpr {
                var =
                (Ast.IndexVar ((1, 33),
                    (Ast.FieldVar ((1, 30), (Ast.SimpleVar ((1, 26), "foo")), "bar")),
                    ((1, 34), (Ast.VarExpr (Ast.SimpleVar ((1, 34), "baz"))))));
                expr = ((1, 42), Ast.NilExpr)})
              ])));


    "create array" >:: (fun _ ->
      assert_ok
        {|foo[2 + 3] of "jj"|}
        ((1, 1),
        Ast.ArrayExpr {ty = "foo";
          size =
          ((1, 7),
            Ast.BinaryExpr {lhs = ((1, 5), (Ast.IntExpr 2)); op = Ast.AddOp;
              rhs = ((1, 9), (Ast.IntExpr 3))});
          init = ((1, 15), (Ast.StrExpr "jj"))}));


    "record" >:: (fun _ ->
      assert_ok
        {|foo { name = "abc", age = 37 * 2, baz = bar {}, biz = F { a = b } }|}
        ((1, 1),
        Ast.RecordExpr {ty = "foo";
          fields =
          [((1, 7), "name", ((1, 14), (Ast.StrExpr "abc")));
            ((1, 21), "age",
              ((1, 30),
              Ast.BinaryExpr {lhs = ((1, 27), (Ast.IntExpr 37)); op = Ast.MulOp;
                rhs = ((1, 32), (Ast.IntExpr 2))}));
            ((1, 35), "baz", ((1, 41), Ast.RecordExpr {ty = "bar"; fields = []}));
            ((1, 49), "biz",
              ((1, 55),
              Ast.RecordExpr {ty = "F";
                fields =
                [((1, 59), "a",
                  ((1, 63), (Ast.VarExpr (Ast.SimpleVar ((1, 63), "b")))))]}))
            ]}));

    "if" >:: (fun _ ->
      assert_ok
        {|(if a > 2 then 1 + 2 else 3; if 3 * 2 - 2 then if -2 then 1+2 else 2/3)|}
        ((1, 1),
        (Ast.SeqExpr
            [((1, 2),
              Ast.IfExpr {
                cond =
                ((1, 7),
                Ast.BinaryExpr {
                  lhs = ((1, 5), (Ast.VarExpr (Ast.SimpleVar ((1, 5), "a"))));
                  op = Ast.GtOp; rhs = ((1, 9), (Ast.IntExpr 2))});
                conseq =
                ((1, 18),
                Ast.BinaryExpr {lhs = ((1, 16), (Ast.IntExpr 1)); op = Ast.AddOp;
                  rhs = ((1, 20), (Ast.IntExpr 2))});
                alt = (Some ((1, 27), (Ast.IntExpr 3)))});
              ((1, 30),
              Ast.IfExpr {
                cond =
                ((1, 39),
                  Ast.BinaryExpr {
                    lhs =
                    ((1, 35),
                    Ast.BinaryExpr {lhs = ((1, 33), (Ast.IntExpr 3));
                      op = Ast.MulOp; rhs = ((1, 37), (Ast.IntExpr 2))});
                    op = Ast.SubOp; rhs = ((1, 41), (Ast.IntExpr 2))});
                conseq =
                ((1, 48),
                  Ast.IfExpr {
                    cond =
                    ((1, 51),
                    Ast.UnaryExpr {op = Ast.NegOp; rand = ((1, 52), (Ast.IntExpr 2))});
                    conseq =
                    ((1, 60),
                    Ast.BinaryExpr {lhs = ((1, 59), (Ast.IntExpr 1));
                      op = Ast.AddOp; rhs = ((1, 61), (Ast.IntExpr 2))});
                    alt =
                    (Some ((1, 69),
                          Ast.BinaryExpr {lhs = ((1, 68), (Ast.IntExpr 2));
                            op = Ast.DivOp; rhs = ((1, 70), (Ast.IntExpr 3))}))});
                alt = None})
              ])));


    "while" >:: (fun _ ->
      assert_ok
        {|while a > 2 do a := a / 7|}
        ((1, 1),
        Ast.WhileExpr {
          cond =
          ((1, 9),
            Ast.BinaryExpr {
              lhs = ((1, 7), (Ast.VarExpr (Ast.SimpleVar ((1, 7), "a"))));
              op = Ast.GtOp; rhs = ((1, 11), (Ast.IntExpr 2))});
          body =
          ((1, 18),
            Ast.AssignExpr {var = (Ast.SimpleVar ((1, 16), "a"));
              expr =
              ((1, 23),
              Ast.BinaryExpr {
                lhs = ((1, 21), (Ast.VarExpr (Ast.SimpleVar ((1, 21), "a"))));
                op = Ast.DivOp; rhs = ((1, 25), (Ast.IntExpr 7))})})}));


    "for" >:: (fun _ ->
      assert_ok
        {|for foo := 1 + 2 to 3 * 2 do bar(foo)|}
        ((1, 1),
        Ast.ForExpr {var = "foo"; escape = ref (true);
          low =
          ((1, 14),
            Ast.BinaryExpr {lhs = ((1, 12), (Ast.IntExpr 1)); op = Ast.AddOp;
              rhs = ((1, 16), (Ast.IntExpr 2))});
          high =
          ((1, 23),
            Ast.BinaryExpr {lhs = ((1, 21), (Ast.IntExpr 3)); op = Ast.MulOp;
              rhs = ((1, 25), (Ast.IntExpr 2))});
          body =
          ((1, 30),
            Ast.CallExpr {func = "bar";
              args = [((1, 34), (Ast.VarExpr (Ast.SimpleVar ((1, 34), "foo"))))]})}));

    
    "break" >:: (fun _ ->
      assert_ok
        {|if 7 * 8 = 1 then (2;3) else break|}
        ((1, 1),
        Ast.IfExpr {
          cond =
          ((1, 10),
            Ast.BinaryExpr {
              lhs =
              ((1, 6),
              Ast.BinaryExpr {lhs = ((1, 4), (Ast.IntExpr 7)); op = Ast.MulOp;
                rhs = ((1, 8), (Ast.IntExpr 8))});
              op = Ast.EqOp; rhs = ((1, 12), (Ast.IntExpr 1))});
          conseq =
          ((1, 19),
            (Ast.SeqExpr [((1, 20), (Ast.IntExpr 2)); ((1, 22), (Ast.IntExpr 3))]));
          alt = (Some ((1, 30), Ast.BreakExpr))}));


    "let" >:: (fun _ ->
      assert_ok
        {|let in let in end end|}
        ((1, 1),
        Ast.LetExpr {decls = [];
          body = ((1, 8), Ast.LetExpr {decls = []; body = ((1, 14), Ast.NilExpr)})}));


    "type alias decl" >:: (fun _ ->
      assert_ok
        {|let type foo = bar in 1; 2 end|}
        ((1, 1),
        Ast.LetExpr {
          decls =
          [(Ast.TypeDecl
              [{ Ast.name = "foo"; ty = (Ast.NameType ((1, 16), "bar"));
                  pos = (1, 5) }
                ])
            ];
          body =
          ((1, 22),
            (Ast.SeqExpr [((1, 23), (Ast.IntExpr 1)); ((1, 26), (Ast.IntExpr 2))]))}));


    "record decl" >:: (fun _ ->
      assert_ok
        {|let type foo = {  } type bar = { a : int, boo : string } in   end|}
        ((1, 1),
        Ast.LetExpr {
          decls =
          [(Ast.TypeDecl
              [{ Ast.name = "foo"; ty = (Ast.RecordType []); pos = (1, 5) };
                { Ast.name = "bar";
                  ty =
                  (Ast.RecordType
                      [{ Ast.name = "a"; ty = "int"; pos = (1, 34) };
                        { Ast.name = "boo"; ty = "string"; pos = (1, 43) }]);
                  pos = (1, 21) }
                ])
            ];
          body = ((1, 60), Ast.NilExpr)}));


    "array decl" >:: (fun _ ->
      assert_ok
        {|let type bar = array of int in end|}
        ((1, 1),
        Ast.LetExpr {
          decls =
          [(Ast.TypeDecl
              [{ Ast.name = "bar"; ty = (Ast.ArrayType ((1, 16), "int"));
                  pos = (1, 5) }
                ])
            ];
          body = ((1, 31), Ast.NilExpr)}));


    "var decl, imp type" >:: (fun _ ->
      assert_ok
        {|let var foo := 2 + 3 in foo end|}
        ((1, 1),
        Ast.LetExpr {
          decls =
          [Ast.VarDecl {name = "foo"; escape = ref (true); ty = None;
              init =
              ((1, 18),
              Ast.BinaryExpr {lhs = ((1, 16), (Ast.IntExpr 2)); op = Ast.AddOp;
                rhs = ((1, 20), (Ast.IntExpr 3))});
              pos = (1, 5)}
            ];
          body = ((1, 25), (Ast.VarExpr (Ast.SimpleVar ((1, 25), "foo"))))}));


    "var decl, exp type" >:: (fun _ ->
      assert_ok
        {|let var foo : string := -4 in end|}
        ((1, 1),
        Ast.LetExpr {
          decls =
          [Ast.VarDecl {name = "foo"; escape = ref (true);
              ty = (Some ((1, 15), "string"));
              init =
              ((1, 25),
              Ast.UnaryExpr {op = Ast.NegOp; rand = ((1, 26), (Ast.IntExpr 4))});
              pos = (1, 5)}
            ];
          body = ((1, 30), Ast.NilExpr)}));


    "fun decl, imp type" >:: (fun _ ->
      assert_ok
        {|let function foo() = 1 + 2 function bar(x : int, y : string) = x - y in end|}
        ((1, 1),
        Ast.LetExpr {
          decls =
          [(Ast.FunDecl
              [{ Ast.name = "foo"; params = []; ret = None;
                  body =
                  ((1, 24),
                  Ast.BinaryExpr {lhs = ((1, 22), (Ast.IntExpr 1)); op = Ast.AddOp;
                    rhs = ((1, 26), (Ast.IntExpr 2))});
                  pos = (1, 5) };
                { Ast.name = "bar";
                  params =
                  [{ Ast.name = "x"; escape = ref (true); ty = "int"; pos = (1, 41)
                      };
                    { Ast.name = "y"; escape = ref (true); ty = "string";
                      pos = (1, 50) }
                    ];
                  ret = None;
                  body =
                  ((1, 66),
                    Ast.BinaryExpr {
                      lhs = ((1, 64), (Ast.VarExpr (Ast.SimpleVar ((1, 64), "x"))));
                      op = Ast.SubOp;
                      rhs = ((1, 68), (Ast.VarExpr (Ast.SimpleVar ((1, 68), "y"))))});
                  pos = (1, 28) }
                ])
            ];
          body = ((1, 72), Ast.NilExpr)}));


    "fun decl, exp type" >:: (fun _ ->
      assert_ok
        {|let function foo(): string = "hh" function bar(xx: int): int = xx + 1 in end|}
        ((1, 1),
        Ast.LetExpr {
          decls =
          [(Ast.FunDecl
              [{ Ast.name = "foo"; params = []; ret = (Some ((1, 21), "string"));
                  body = ((1, 30), (Ast.StrExpr "hh")); pos = (1, 5) };
                { Ast.name = "bar";
                  params =
                  [{ Ast.name = "xx"; escape = ref (true); ty = "int"; pos = (1, 48)
                      }
                    ];
                  ret = (Some ((1, 58), "int"));
                  body =
                  ((1, 67),
                    Ast.BinaryExpr {
                      lhs = ((1, 64), (Ast.VarExpr (Ast.SimpleVar ((1, 64), "xx"))));
                      op = Ast.AddOp; rhs = ((1, 69), (Ast.IntExpr 1))});
                  pos = (1, 35) }
                ])
            ];
          body = ((1, 73), Ast.NilExpr)}));


    "mixed decl" >:: (fun _ ->
      assert_ok
        {||}
        ((1, 1), NilExpr));


    "mixed" >:: (fun _ ->
      assert_ok
        {||}
        ((1, 1), NilExpr))
  ]

let () =
  run_test_tt_main test_parse