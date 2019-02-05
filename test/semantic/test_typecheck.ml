open Base
open OUnit2
open Lex
open Parse
open Semantic
open Stdio


let run_analyzer s =
  let temp_file = Caml.Filename.temp_file "tiger_semantic_" "" in
  let oc = Out_channel.create ~append:false temp_file in
  Errors.set_out oc;
  let expr = Parser.prog Lexer.get_token (Lexing.from_string s) in
  let ty = Analyzer.transProg (module Dummy_frame) expr in
  Out_channel.close oc;
  let errors = In_channel.read_all temp_file in
  (ty, errors)

let assert_ok (src : string) (expected_ty : Type.t) =
  let (actual_ty, actual_errors) = run_analyzer src in
  if Poly.(actual_errors = "") && Type.eq_ignore_unique actual_ty expected_ty then
    ()
  else
    assert_failure
      (Printf.sprintf {|=========== expected ==========

type: %s
no errors.

========== actual ==========

type: %s
errors:
%s
|}
         (Type.show expected_ty)
         (Type.show actual_ty)
         actual_errors)

let assert_error (src : string) (expected_errors : string) =
  let expected_errors = String.lstrip expected_errors in
  let (actual_ty, actual_errors) = run_analyzer src in
  if Poly.(actual_errors = expected_errors) then
    ()
  else
    assert_failure
      (Printf.sprintf {|=========== expected ==========

errors:
%s

========== actual ==========

type: %s
errors:
%s
|}
         expected_errors
         (Type.show actual_ty)
         actual_errors)

let test_semantic =
  let dummy_unique = Type.new_unique (Type.new_unique_store ()) in
  "Test Typechecker" >::: [

    "value" >::: [
      "nil" >:: (fun _ ->
        assert_ok
          {|nil|}
          Type.NilType);

      "unit" >:: (fun _ ->
        assert_ok
          {|()|}
          Type.UnitType);

      "int" >:: (fun _ ->
        assert_ok
          {|123|}
          Type.IntType);

      "string" >:: (fun _ ->
        assert_ok
          {|"abc"|}
          Type.StringType);

      "array" >:: (fun _ ->
        assert_ok
          {|let type a = array of int in a[0] of 30 end|}
          (Type.ArrayType (Type.IntType, dummy_unique)));

      "record" >:: (fun _ ->
        assert_ok
          {|let type a = {x:int} in a{x=20} end|}
          (Type.RecordType ([(Symbol.sym "x", Type.IntType)], dummy_unique)));
    ];

    "unary" >::: [
      "negation" >:: (fun _ ->
        assert_ok
          {|- 30|}
          Type.IntType);

      "negation errors" >:: (fun _ ->
        assert_error
          {|(-"abc"; -())|}
          {|
Error at (1:3): expected int type for operand of negation, got string type
Error at (1:11): expected int type for operand of negation, got unit type
|});
    ];

    "binary" >::: [
      "arithmetic" >:: (fun _ ->
        assert_ok
          {|3+4-6*7/80|}
          Type.IntType);

      "arithmetic errors" >:: (fun _ ->
        assert_error
          {|(3+()-("a"*20)/80)|}
          {|
Error at (1:4): expected int type for right-hand side of '+', got unit type
Error at (1:8): expected int type for left-hand side of '*', got string type
|});

      "comparison" >::: [
        "int" >:: (fun _ ->
          assert_ok
            {|3 > 2|}
            Type.IntType);

        "string" >:: (fun _ ->
          assert_ok
            {|("what" <= "abc") + ("a" = "g")|}
            Type.IntType);

        "array" >:: (fun _ ->
          assert_ok
            {|let type a = array of int var t := a[3] of 0 in t <> a[0] of 30 end|}
            Type.IntType);

        "record" >:: (fun _ ->
          assert_ok
            {|let type a = { } var t := a { } in t = a { } end|}
            Type.IntType);

        "record vs nil" >:: (fun _ ->
          assert_ok
            {|let type a = { } var t := a { } in nil <> t; t = nil end|}
            Type.IntType)
      ];

      "comparison errors" >:: (fun _ ->
        assert_error
          {|
let
  type arr1 = array of int
  type arr2 = array of string
  type arr3 = array of int
  
  type rec1 = {  }
  type rec2 = { aa : int }
  type rec3 = {  }

  var t1 := arr1[1] of 0
  var t2 := rec1 {}
in
  1 = "abc";
  () = ();
  t1 > t1;
  t1 = arr2[0] of "a";
  arr3[0] of 0 <> t1;
  t2 = rec2 { aa = 2 };
  rec3 {} = t2;
  nil = nil
end
          |}
          {|
Error at (14:5): incompatible operand types for '='
Error at (15:3): expected int, string, record or array type for left-hand side of '=', got unit type
Error at (15:8): expected int, string, record or array type for right-hand side of '=', got unit type
Error at (16:3): expected int or string type for left-hand side of '>', got (array of int)#0 type
Error at (16:8): expected int or string type for right-hand side of '>', got (array of int)#0 type
Error at (17:6): incompatible operand types for '='
Error at (18:16): incompatible operand types for '<>'
Error at (19:6): incompatible operand types for '='
Error at (20:11): incompatible operand types for '='
Error at (21:7): cannot determine the types of the operands
|});

      "and/or" >:: (fun _ ->
        assert_ok
          {|3 & (2 <> 1) | -1|}
          Type.IntType);

      "and/or errors" >:: (fun _ ->
        assert_error
          {|let type a = array of int in "a" & () | a[0] of 1 end|}
          {|
Error at (1:30): expected int type for left-hand side of '&', got string type
Error at (1:36): expected int type for right-hand side of '&', got unit type
Error at (1:41): expected int type for right-hand side of '|', got (array of int)#0 type
|});
    ];

    "variable" >:: (fun _ ->
      assert_ok
        {|let var a := "a" in a end|}
        Type.StringType);

    "variable errors" >:: (fun _ ->
      assert_error
        {|
(a;
let function b() = () in b end)
|}
        {|
Error at (2:2): undefined variable: a
Error at (3:26): cannot use function as variable: b
|});

    "index" >:: (fun _ ->
      assert_ok
        {|let type a = array of int var t := a[0] of -1 in t[20] end|}
        Type.IntType);

    "index errors" >:: (fun _ ->
      assert_error
        {|
let
type a = array of string
var t := 3
var r := a[0] of ""
in
a[20];
t["w"];
r[()]
end
|}
        {|
Error at (7:1): undefined variable: a
Error at (7:2): cannot index on int type
Error at (8:3): expected int type for index, got string type
Error at (8:2): cannot index on int type
Error at (9:3): expected int type for index, got unit type
|});

    "field selection" >:: (fun _ ->
      assert_ok
        {|
let
  type a1 = array of int
  type r1 = { bar : int, baz : a1 }
  var t : r1 := nil
in
  t.baz
end
  |}
        (Type.ArrayType (Type.IntType, dummy_unique)));

    "field selection errors" >:: (fun _ ->
      assert_error
        {|
let
  type r = { bar : string, fo : int }
  var b := ()
  var c := r { bar="", fo=2 }
in
  a.foo;
  b.foo;
  c.foo
end
        |}
        {|
Error at (7:3): undefined variable: a
Error at (7:5): expected record type for field selection, got int type
Error at (8:5): expected record type for field selection, got unit type
Error at (9:5): record { bar : string, fo : int }#0 type has no field named foo
|});

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
  a.foo[2 + 3 * 8].bar
end
        |}
        (Type.ArrayType (Type.IntType, dummy_unique)));

    "assignment" >::: [
      "int" >:: (fun _ ->
        assert_ok
          {|let var t := 20 in t := 27 end|}
          Type.UnitType);

      "unit" >:: (fun _ ->
        assert_ok
          {|let var t := () in t := () end|}
          Type.UnitType);

      "array" >:: (fun _ ->
        assert_ok
          {|let type a = array of int var t := a[0] of 0 in t := a[2] of -3 end|}
          Type.UnitType);

      "index" >:: (fun _ ->
        assert_ok
          {|let type a = array of int var t := a[0] of 0 in t[2] := 7 end|}
          Type.UnitType);

      "record" >:: (fun _ ->
        assert_ok
          {|let type r = { foo : string } var t := r { foo = "q" } in t := r { foo = "123" } end|}
          Type.UnitType);

      "nil record" >:: (fun _ ->
        assert_ok
          {|let type r = { foo : string } var t := r { foo = "q" } in t := nil end|}
          Type.UnitType);

      "field" >:: (fun _ ->
        assert_ok
          {|let type r = { foo : string } var t := r { foo = "q" } in t.foo := "wh" end|}
          Type.UnitType);

      "errors" >:: (fun _ ->
        assert_error
          {|
let
  var t := 20
  type r1 = { foo : int }
  type r2 = { foo : int }
  var m : r1 := nil
in
  a := 20;
  t := "a";
  m := r2 { foo = 20 };
  for i := 1 to 2 do i := 2
end
          |}
          {|
Error at (8:3): undefined variable: a
Error at (9:8): expected int type for right-hand side of assignment, got string type
Error at (10:8): expected record { foo : int }#0 type for right-hand side of assignment, got record { foo : int }#1 type
Error at (11:22): variable cannot be assigned to: i
|})
    ];

    "record creation" >:: (fun _ ->
      assert_ok
        {|
let
  type rec0 = { a : rec1, foo : arr }
  type rec1 = { b : rec2, bar : arr }
  type rec2 = { }
  type arr = array of int
  var a1 := arr[2] of 3
in
  rec0 { a = rec1 { b = nil, bar = a1 }, foo = a1 }
end
        |}
        (Type.RecordType ([
          (Symbol.sym "a", Type.RecordType ([
            (Symbol.sym "b", Type.RecordType ([], dummy_unique));
            (Symbol.sym "bar", Type.ArrayType (Type.IntType, dummy_unique))
          ], dummy_unique));
          (Symbol.sym "foo", Type.ArrayType (Type.IntType, dummy_unique))], dummy_unique)));

    "record creation errors" >:: (fun _ ->
      assert_error
        {|
let
  type rec = { foo : int, bar : string, baz : rec }
  var t := rec {}
in
  r {};
  int {};
  rec { foo = () };
  rec { foo = "", bar="", baz=t };
  rec { foo=2, baz=nil, bar="" }
end
        |}
        {|
Error at (4:12): expected 3 fields, got 0 fields
Error at (6:3): undefined record type: r
Error at (7:3): expected record type for record creation, got int type
Error at (8:3): expected 3 fields, got 1 fields
Error at (8:9): expected field 'foo' of int type, got field 'foo' of unit type
Error at (9:9): expected field 'foo' of int type, got field 'foo' of string type
Error at (10:16): expected field 'bar' of string type, got field 'baz' of nil type
Error at (10:25): expected field 'baz' of record { foo : int, bar : string, baz : alias 'rec' }#0 type, got field 'bar' of string type
|});

    "array creation" >:: (fun _ ->
      assert_ok
        {|
let
  type a1 = array of a2
  type a2 = array of int
  var a := a1[0] of (a2[0] of 0)
in
  a
end
        |}
        (Type.ArrayType (Type.ArrayType (Type.IntType, dummy_unique), dummy_unique)));

    "array creation errors" >:: (fun _ ->
      assert_error
        {|
let
  type a1 = array of int
  var t1 := z[0] of ""
  var t2 := a1[0] of ""
in
  a1[()] of -1;
  int[0] of 0
end
        |}
        {|
Error at (4:13): undefined array type: z
Error at (5:13): expected int type for initial value of array, got string type
Error at (7:6): expected int type for array size, got unit type
Error at (8:3): expected array type for array creation, got int type
|});

    "if" >::: [
      "if-then-else" >:: (fun _ ->
        assert_ok
          {|if 2 > 3 then 1 else 30|}
          Type.IntType);

      "if-then" >:: (fun _ ->
        assert_ok
          {|let var a := 3 in if a > 2 then a := 30 end|}
          Type.UnitType)
    ];

    "if errors" >:: (fun _ ->
      assert_error
        {|
(if "" then 2 else nil;
 if 0 then 20)
        |}
        {|
Error at (2:2): expected int type for condition of if expression, got string type
Error at (2:2): incompatible branch types for if expression
Error at (3:2): incompatible branch types for if expression
|});

    "function call" >:: (fun _ ->
      assert_ok
        {|
let
  type rec = { bar : string }
  function foo(x : rec, y : rec) : string = x.bar
in
  substring(concat(getchar(), foo(nil, rec { bar = "ww" })), 20, 1)
end
        |}
        Type.StringType);

    "function call errors" >:: (fun _ ->
      assert_error
        {|
let
  type rec1 = { bar : int }
  type rec2 = { bar : int }
  function foo(x : rec1) : int = x.bar
  var f := 2
in
  substring(chr(f(), f), foo(rec2 { bar = 2 }), bar())
end
        |}
        {|
Error at (8:17): cannot call non-function variable: f
Error at (8:13): expected 1 parameters, got 2 arguments
Error at (8:30): expected record { bar : int }#0 type for argument 1, got record { bar : int }#1 type
Error at (8:49): undefined function: bar
|});

    "while loop" >:: (fun _ ->
      assert_ok
        {|let var i := 2 in while i < 10 do i := i + 1 end|}
        Type.UnitType);

    "while loop errors" >:: (fun _ ->
      assert_error
        {|while "a" do 30|}
        {|
Error at (1:1): expected int type for condition of while loop, got string type
Error at (1:1): expected unit type for body of while loop, got int type
|});

    "for loop" >:: (fun _ ->
      assert_ok
        {|for i := 1 to 10 do print("")|}
        Type.UnitType);

    "for loop errors" >:: (fun _ ->
      assert_error
        {|for abc := "" to () do 328|}
        {|
Error at (1:1): expected int type for lower bound of for loop, got string type
Error at (1:1): expected int type for higher bound of for loop, got unit type
Error at (1:1): expected unit type for body of for loop, got int type
|});

    "break in while loop" >:: (fun _ ->
      assert_ok
        {|
while 1 do
(let
var a := (break; 1)
type rec = { foo : int }
var b : rec := nil
in
if a > 2 then break;
break;
b := rec { foo = (break; 2) }
end;
break)
        |}
        Type.UnitType);

    "break in for loop" >:: (fun _ ->
      assert_ok
        {|
for aa := 1 to 0 do
(let
var a := (break; 1)
type rec = { foo : int }
var b : rec := nil
in
if a > 2 then break;
break;
b := rec { foo = (break; 2) }
end;
break)
        |}
        Type.UnitType);

    "break errors" >:: (fun _ ->
      assert_error
        {|
(while 0 do let
  function foo() = break
in
end;
break)
        |}
        {|
Error at (3:20): break expression must be inside loop
Error at (6:1): break expression must be inside loop
|});

    "let" >::: [
      "cyclic type" >:: (fun _ ->
        assert_ok
          {|
let
  type foo = { a : int, b : bar }
  type bar = { a : string, b : foo }
in
end
          |}
          Type.UnitType);

      "type decl errors" >:: (fun _ ->
        assert_error
          {|
let
  type a = { a : int, a : string }
  type b = c
  type c = d
  type d = b
  type a = b
in
end
          |}
          {|
Error at (3:23): duplicate field: a
Error at (7:3): duplicate type declaration: a
Error at (7:3): type alias cycle detected: b, d, c
|});

      "var decl shadowing" >:: (fun _ ->
        assert_ok
          {|
let
  var f := 20
  var x := "s"
  var x := 123
  function f(x : int, y : string) = ()
in
  f(x, let var x := "ww" in x end)
end
          |}
          Type.UnitType);

      "var decl errors" >:: (fun _ ->
        assert_error
          {|
let
  type arr1 = array of int
  type arr2 = array of int
  var t : integer := 20
  var t := nil
  var t1 : string := 20
  var t2 : arr1 := arr2[1] of 0
in
end
          |}
          {|
Error at (5:11): undefined type: integer
Error at (6:3): variable declaration must specify type for nil initial value
Error at (7:3): expected string type for variable initialization, got int type
Error at (8:3): expected (array of int)#0 type for variable initialization, got (array of int)#1 type
|});

      "mutual recursive functions" >:: (fun _ ->
        assert_ok
          {|
let
  type integer = int
  function even(x : integer) : int =
    if x = 0 then 1 else odd(x - 1)
  function odd(x : int) : integer =
    if x = 0 then 0 else even(x - 1)
in
  even(odd(20))
end
          |}
          Type.IntType);

      "function decl errors" >:: (fun _ ->
        assert_error
          {|
let
  function foo(x : int) = ()
  function foo(x : string) : int = 30
  function bar(x : int, x : string) : int = nil
in
  foo(20)
end
          |}
          {|
Error at (4:3): duplicate function declaration: foo
Error at (5:25): duplicate paramater: x
Error at (5:3): expected int type for function body, got nil type
Error at (7:7): expected string type for argument 1, got int type
|})
    ]
  ]

let () =
  run_test_tt_main test_semantic