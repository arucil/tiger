open Base
open OUnit2
open Parse
open Lex
open Ast
open Semantic
open Common

let run_find_escape s =
  let ast = Parser.prog Lexer.get_token (Lexing.from_string s) in
  Find_escape.find_escape ast;
  ast

let assert_ok (src : string) (expected : expr) =
  let actual = run_find_escape src in
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

let test_find_escape =
  "Test Find Escape" >::: [
    "var escape" >:: (fun _ ->
      assert_ok
        {|
let
  var a := 30
  var b := 40
  var c := ()
  var d := "a"
  var e := "how"
  function foo(x:int) =
    ((if 12 then c := 2 else
      let
        function bar(e: int) = (d; e := 2)
      in
      end;
      -1) + (-(b*4)); ())
in
  foo(a)
end
        |}
        ((2, 1),
 Ast.LetExpr {
   decls =
   [Ast.VarDecl {name = Symbol.sym "a"; escape = ref (false); ty = None;
      init = ((3, 12), (Ast.IntExpr 30l)); pos = (3, 3)};
     Ast.VarDecl {name = Symbol.sym "b"; escape = ref (true); ty = None;
       init = ((4, 12), (Ast.IntExpr 40l)); pos = (4, 3)};
     Ast.VarDecl {name = Symbol.sym "c"; escape = ref (true); ty = None;
       init = ((5, 12), (Ast.SeqExpr [])); pos = (5, 3)};
     Ast.VarDecl {name = Symbol.sym "d"; escape = ref (true); ty = None;
       init = ((6, 12), (Ast.StrExpr "a")); pos = (6, 3)};
     Ast.VarDecl {name = Symbol.sym "e"; escape = ref (false); ty = None;
       init = ((7, 12), (Ast.StrExpr "how")); pos = (7, 3)};
     (Ast.FunDecl
        [{ Ast.name = Symbol.sym "foo";
           params =
           [{ Ast.name = Symbol.sym "x"; escape = ref (false); ty = Symbol.sym "int"; pos = (8, 16) }];
           ret = None;
           body =
           ((9, 5),
            (Ast.SeqExpr
               [((14, 11),
                 Ast.BinaryExpr {
                   lhs =
                   ((9, 6),
                    (Ast.SeqExpr
                       [((9, 7),
                         Ast.IfExpr {cond = ((9, 10), (Ast.IntExpr 12l));
                           conseq =
                           ((9, 20),
                            Ast.AssignExpr {
                              var = (Ast.SimpleVar ((9, 18), Symbol.sym "c"));
                              expr = ((9, 23), (Ast.IntExpr 2l))});
                           alt =
                           (Some ((10, 7),
                                  Ast.LetExpr {
                                    decls =
                                    [(Ast.FunDecl
                                        [{ Ast.name = Symbol.sym "bar";
                                           params =
                                           [{ Ast.name = Symbol.sym "e";
                                              escape = ref (false); ty = Symbol.sym "int";
                                              pos = (11, 22) }
                                             ];
                                           ret = None;
                                           body =
                                           ((11, 32),
                                            (Ast.SeqExpr
                                               [((11, 33),
                                                 (Ast.VarExpr
                                                    (Ast.SimpleVar ((
                                                       11, 33), Symbol.sym "d"))));
                                                 ((11, 38),
                                                  Ast.AssignExpr {
                                                    var =
                                                    (Ast.SimpleVar ((
                                                       11, 36), Symbol.sym "e"));
                                                    expr =
                                                    ((11, 41),
                                                     (Ast.IntExpr 2l))})
                                                 ]));
                                           pos = (11, 9) }
                                          ])
                                      ];
                                    body = ((12, 9), (Ast.SeqExpr []))}))});
                         ((14, 7),
                          Ast.UnaryExpr {op = Ast.NegOp;
                            rand = ((14, 8), (Ast.IntExpr 1l))})
                         ]));
                   op = Ast.AddOp;
                   rhs =
                   ((14, 14),
                    Ast.UnaryExpr {op = Ast.NegOp;
                      rand =
                      ((14, 17),
                       Ast.BinaryExpr {
                         lhs =
                         ((14, 16),
                          (Ast.VarExpr (Ast.SimpleVar ((14, 16), Symbol.sym "b"))));
                         op = Ast.MulOp; rhs = ((14, 18), (Ast.IntExpr 4l))})})});
                 ((14, 23), (Ast.SeqExpr []))]));
           pos = (8, 3) }
          ])
     ];
   body =
   ((16, 3),
    Ast.CallExpr {func = Symbol.sym "foo";
      args = [((16, 7), (Ast.VarExpr (Ast.SimpleVar ((16, 7), Symbol.sym "a"))))]})}));

    "param escape" >:: (fun _ ->
      assert_ok
        {|
let
  type arr = array of int
  function foo(x:int,y:string,z:arr,w:int) =
    let
      function bar() = ()
      function baz(w:int,a:string) =
        (concat(a,y)*w;
         fj[z+3] := 23)
    in
      while x < 12 do
       x := x + 12
    end
in
end
        |}
        ((2, 1),
 Ast.LetExpr {
   decls =
   [(Ast.TypeDecl
       [{ Ast.name = Symbol.sym "arr"; ty = (Ast.ArrayType ((3, 14), Symbol.sym "int")); pos = (3, 3) }
         ]);
     (Ast.FunDecl
        [{ Ast.name = Symbol.sym "foo";
           params =
           [{ Ast.name = Symbol.sym "x"; escape = ref (false); ty = Symbol.sym "int"; pos = (4, 16) };
             { Ast.name = Symbol.sym "y"; escape = ref (true); ty = Symbol.sym "string"; pos = (4, 22)
               };
             { Ast.name = Symbol.sym "z"; escape = ref (true); ty = Symbol.sym "arr"; pos = (4, 31) };
             { Ast.name = Symbol.sym "w"; escape = ref (false); ty = Symbol.sym "int"; pos = (4, 37) }];
           ret = None;
           body =
           ((5, 5),
            Ast.LetExpr {
              decls =
              [(Ast.FunDecl
                  [{ Ast.name = Symbol.sym "bar"; params = []; ret = None;
                     body = ((6, 24), (Ast.SeqExpr [])); pos = (6, 7) };
                    { Ast.name = Symbol.sym "baz";
                      params =
                      [{ Ast.name = Symbol.sym "w"; escape = ref (false); ty = Symbol.sym "int";
                         pos = (7, 20) };
                        { Ast.name = Symbol.sym "a"; escape = ref (false); ty = Symbol.sym "string";
                          pos = (7, 26) }
                        ];
                      ret = None;
                      body =
                      ((8, 9),
                       (Ast.SeqExpr
                          [((8, 21),
                            Ast.BinaryExpr {
                              lhs =
                              ((8, 10),
                               Ast.CallExpr {func = Symbol.sym "concat";
                                 args =
                                 [((8, 17),
                                   (Ast.VarExpr (Ast.SimpleVar ((8, 17), Symbol.sym "a"))));
                                   ((8, 19),
                                    (Ast.VarExpr (Ast.SimpleVar ((8, 19), Symbol.sym "y"))))
                                   ]});
                              op = Ast.MulOp;
                              rhs =
                              ((8, 22),
                               (Ast.VarExpr (Ast.SimpleVar ((8, 22), Symbol.sym "w"))))});
                            ((9, 18),
                             Ast.AssignExpr {
                               var =
                               (Ast.IndexVar ((9, 12),
                                  (Ast.SimpleVar ((9, 10), Symbol.sym "fj")),
                                  ((9, 14),
                                   Ast.BinaryExpr {
                                     lhs =
                                     ((9, 13),
                                      (Ast.VarExpr
                                         (Ast.SimpleVar ((9, 13), Symbol.sym "z"))));
                                     op = Ast.AddOp;
                                     rhs = ((9, 15), (Ast.IntExpr 3l))})
                                  ));
                               expr = ((9, 21), (Ast.IntExpr 23l))})
                            ]));
                      pos = (7, 7) }
                    ])
                ];
              body =
              ((11, 7),
               Ast.WhileExpr {
                 cond =
                 ((11, 15),
                  Ast.BinaryExpr {
                    lhs =
                    ((11, 13), (Ast.VarExpr (Ast.SimpleVar ((11, 13), Symbol.sym "x"))));
                    op = Ast.LtOp; rhs = ((11, 17), (Ast.IntExpr 12l))});
                 body =
                 ((12, 10),
                  Ast.AssignExpr {var = (Ast.SimpleVar ((12, 8), Symbol.sym "x"));
                    expr =
                    ((12, 15),
                     Ast.BinaryExpr {
                       lhs =
                       ((12, 13), (Ast.VarExpr (Ast.SimpleVar ((12, 13), Symbol.sym "x"))));
                       op = Ast.AddOp; rhs = ((12, 17), (Ast.IntExpr 12l))})})})});
           pos = (4, 3) }
          ])
     ];
    body = ((14, 3), (Ast.SeqExpr []))}));

    "var escape of for-loop" >:: (fun _ ->
      assert_ok
        {|
for i := 1 + 2 to 30 do
  for j := 1 to 20 do
    for k := 0 to 0 do
      for i := 1 to 0 do
        let
          function foo() =
            (i := k + foo(j))
        in
        end
        |}
        ((2, 1),
 Ast.ForExpr {var = Symbol.sym "i"; escape = ref (false);
   low =
   ((2, 12),
    Ast.BinaryExpr {lhs = ((2, 10), (Ast.IntExpr 1l)); op = Ast.AddOp;
      rhs = ((2, 14), (Ast.IntExpr 2l))});
   high = ((2, 19), (Ast.IntExpr 30l));
   body =
   ((3, 3),
    Ast.ForExpr {var = Symbol.sym "j"; escape = ref (true);
      low = ((3, 12), (Ast.IntExpr 1l)); high = ((3, 17), (Ast.IntExpr 20l));
      body =
      ((4, 5),
       Ast.ForExpr {var = Symbol.sym "k"; escape = ref (true);
         low = ((4, 14), (Ast.IntExpr 0l)); high = ((4, 19), (Ast.IntExpr 0l));
         body =
         ((5, 7),
          Ast.ForExpr {var = Symbol.sym "i"; escape = ref (true);
            low = ((5, 16), (Ast.IntExpr 1l));
            high = ((5, 21), (Ast.IntExpr 0l));
            body =
            ((6, 9),
             Ast.LetExpr {
               decls =
               [(Ast.FunDecl
                   [{ Ast.name = Symbol.sym "foo"; params = []; ret = None;
                      body =
                      ((8, 16),
                       Ast.AssignExpr {var = (Ast.SimpleVar ((8, 14), Symbol.sym "i"));
                         expr =
                         ((8, 21),
                          Ast.BinaryExpr {
                            lhs =
                            ((8, 19),
                             (Ast.VarExpr (Ast.SimpleVar ((8, 19), Symbol.sym "k"))));
                            op = Ast.AddOp;
                            rhs =
                            ((8, 23),
                             Ast.CallExpr {func = Symbol.sym "foo";
                               args =
                               [((8, 27),
                                 (Ast.VarExpr (Ast.SimpleVar ((8, 27), Symbol.sym "j"))))]})})});
                      pos = (7, 11) }
                     ])
                 ];
               body = ((9, 11), (Ast.SeqExpr []))})})})})}));
  ]

let () =
  run_test_tt_main test_find_escape