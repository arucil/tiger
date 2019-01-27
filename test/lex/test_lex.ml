
open Base
open OUnit2
open Lex
open Token

let run_lexer s =
  let lexbuf = Lexing.from_string s in
  let rec go () =
    match Lexer.get_token lexbuf with
    | Token.EOF _ as t -> [t]
    | t -> t :: go ()
  in
    go ()

let assert_ok (src : string) (expected : token list) =
  let actual = run_lexer src in
  if Poly.(actual = expected) then
    ()
  else
    assert_failure
      (Printf.sprintf {|=========== expected ==========

%s

========== actual ==========

%s
|}
         ([%derive.show: token list] expected)
         ([%derive.show: token list] actual))

let test_lex =
  "Test Lexer" >::: [


    "keywords" >:: (fun _ ->
      assert_ok {|
  while  for      to
  break  let      in
  end    function var
  type   array    if
  then   else     do
  of     nil
|}
      [
        WHILE (2, 3); FOR (2, 10); TO (2, 19);
        BREAK (3, 3); LET (3, 10); IN (3, 19);
        END (4, 3); FUNCTION (4, 10); VAR (4, 19);
        TYPE (5, 3); ARRAY (5, 10); IF (5, 19);
        THEN (6, 3); ELSE (6, 10); DO (6, 19);
        OF (7, 3); NIL (7, 10);
        EOF (8, 1);
      ]);


    "operators" >:: (fun _ ->
      assert_ok {|
  +-*/=:=,:;()[]{}>>=<<><=|&
|}
      [
        PLUS (2, 3); MINUS (2, 4); TIMES (2, 5); DIV (2, 6);
        EQ (2, 7); ASSIGN (2, 8); COMMA (2, 10); COLON (2, 11);
        SEMI (2, 12); LPAREN (2, 13); RPAREN (2, 14); LBRACK (2, 15);
        RBRACK (2, 16); LBRACE (2, 17); RBRACE (2, 18); GT (2, 19);
        GE (2, 20); LT (2, 22); NEQ (2, 23); LE (2, 25); OR (2, 27);
        AND (2, 28);
        EOF (3, 1)
      ]);


    "identifier" >:: (fun _ ->
      assert_ok {|
abc123  _Hello  __
a_B_c_1_2_3_.L1+If
|}
      [
        ID ((2, 1), "abc123"); ID ((2, 9), "_Hello"); ID ((2, 17), "__");
        ID ((3, 1), "a_B_c_1_2_3_"); DOT (3, 13);
        ID ((3, 14), "L1"); PLUS (3, 16); ID ((3, 17), "If");
        EOF (4, 1)
      ]);


    "number" >:: (fun _ ->
      assert_ok {| 123  5624214   -000234 000
12000 |}
        [
          INT ((1, 2), 123); INT ((1, 7), 5624214);
          MINUS (1, 17);
          INT ((1, 18), 234); INT ((1, 25), 0);
          INT ((2, 1), 12000);
          EOF (2, 7)
        ]);


    "comment" >::: [
      "plain" >:: (fun _ ->
        assert_ok {|
  /* */
  /*comment*/   /***//***
****/
|}
          [
            EOF (5, 1)
          ];
        assert_ok {|127
/*  outer* / * /*
 inner 
 */*
* outer** */2
|}
          [
            INT ((1, 1), 127); INT ((5, 13), 2);
            EOF (6, 1)
          ])];


    "string" >::: [
      "plain" >:: (fun _ ->
        assert_ok {|
"abc""hey_12 +-"  "Wow!"
|}
          [
            STR ((2, 1), "abc");
            STR ((2, 6), "hey_12 +-");
            STR ((2, 19), "Wow!");
            EOF (3, 1)
          ]
      );

      "char escape" >:: (fun _ ->
        assert_ok {|
"ok:\"\\abc\"" "ABC\tDEF\nxyz"
|}
          [
            STR ((2, 1), {|ok:"\abc"|});
            STR ((2, 16), "ABC\tDEF\nxyz");
            EOF (3, 1)
          ]
      );

      "ascii escape" >:: (fun _ ->
        assert_ok {|
"\097b\0999d"
|}
          [
            STR ((2, 1), "abc9d"); EOF (3, 1)
          ]
      );

      "line-spanning escape" >:: (fun _ ->
        assert_ok {|
"abc\      \xyz"
  "abc\
  \def\
  \ghi"  "123\


            \456"
|}
          [
            STR ((2, 1), "abcxyz");
            STR ((3, 3), "abcdefghi");
            STR ((5, 10), "123456");
            EOF (9, 1)
          ]
        );
    ];

    "mixed" >:: (fun _ ->
      assert_ok {|
let
  var foo : int := 30
  function fib(x : int) : int =
    if x < 2 then x else fib(x - 1) + fib(x - 2)
in
  fib(foo)
end
|}
        [
          (Token.LET (2, 1)); (Token.VAR (3, 3)); (Token.ID ((3, 7), "foo"));
          (Token.COLON (3, 11)); (Token.ID ((3, 13), "int")); (Token.ASSIGN (3, 17));
          (Token.INT ((3, 20), 30)); (Token.FUNCTION (4, 3));
          (Token.ID ((4, 12), "fib")); (Token.LPAREN (4, 15));
          (Token.ID ((4, 16), "x")); (Token.COLON (4, 18));
          (Token.ID ((4, 20), "int")); (Token.RPAREN (4, 23)); (Token.COLON (4, 25));
          (Token.ID ((4, 27), "int")); (Token.EQ (4, 31)); (Token.IF (5, 5));
          (Token.ID ((5, 8), "x")); (Token.LT (5, 10)); (Token.INT ((5, 12), 2));
          (Token.THEN (5, 14)); (Token.ID ((5, 19), "x")); (Token.ELSE (5, 21));
          (Token.ID ((5, 26), "fib")); (Token.LPAREN (5, 29));
          (Token.ID ((5, 30), "x")); (Token.MINUS (5, 32)); (Token.INT ((5, 34), 1));
          (Token.RPAREN (5, 35)); (Token.PLUS (5, 37)); (Token.ID ((5, 39), "fib"));
          (Token.LPAREN (5, 42)); (Token.ID ((5, 43), "x")); (Token.MINUS (5, 45));
          (Token.INT ((5, 47), 2)); (Token.RPAREN (5, 48)); (Token.IN (6, 1));
          (Token.ID ((7, 3), "fib")); (Token.LPAREN (7, 6));
          (Token.ID ((7, 7), "foo")); (Token.RPAREN (7, 10)); (Token.END (8, 1));
          (Token.EOF (9, 1))
        ]
    );
  ]

let () =
  run_test_tt_main test_lex