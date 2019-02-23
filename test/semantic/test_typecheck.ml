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
  Errors.set_source_name "-";
  let expr = Parser.prog Lexer.get_token (Lexing.from_string s) in
  let trans = (module Translate.Make(Dummy_platf) : Translate.S) in
  let temp_store = Temp.new_store () in
  let (ty, _) = Analyzer.trans_prog expr trans temp_store in
  Out_channel.close oc;
  let errors = In_channel.read_all temp_file in
  (ty, errors)

let assert_ok (src : string) (expected_ty : Type.t) =
  let (actual_ty, actual_errors) = run_analyzer src in
  if String.(actual_errors = "") && Type.eq_ignore_unique actual_ty expected_ty then
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
  if String.(actual_errors = expected_errors) then
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

let test_typecheck =
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
-:1:3: error: expected int type for operand of negation, got string type
-:1:11: error: expected int type for operand of negation, got unit type
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
-:1:4: error: expected int type for right-hand side of '+', got unit type
-:1:8: error: expected int type for left-hand side of '*', got string type
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
-:14:5: error: incompatible operand types for '='
-:15:3: error: expected int, string, record or array type for left-hand side of '=', got unit type
-:15:8: error: expected int, string, record or array type for right-hand side of '=', got unit type
-:16:3: error: expected int or string type for left-hand side of '>', got (array of int)#0 type
-:16:8: error: expected int or string type for right-hand side of '>', got (array of int)#0 type
-:17:6: error: incompatible operand types for '='
-:18:16: error: incompatible operand types for '<>'
-:19:6: error: incompatible operand types for '='
-:20:11: error: incompatible operand types for '='
-:21:7: error: cannot determine the types of the operands
|});

      "and/or" >:: (fun _ ->
        assert_ok
          {|3 & (2 <> 1) | -1|}
          Type.IntType);

      "and/or errors" >:: (fun _ ->
        assert_error
          {|let type a = array of int in "a" & () | a[0] of 1 end|}
          {|
-:1:30: error: expected int type for left-hand side of '&', got string type
-:1:36: error: expected int type for right-hand side of '&', got unit type
-:1:41: error: expected int type for right-hand side of '|', got (array of int)#0 type
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
-:2:2: error: undefined variable: a
-:3:26: error: cannot use function as variable: b
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
-:7:1: error: undefined variable: a
-:7:2: error: cannot index on int type
-:8:3: error: expected int type for index, got string type
-:8:2: error: cannot index on int type
-:9:3: error: expected int type for index, got unit type
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
-:7:3: error: undefined variable: a
-:7:5: error: expected record type for field selection, got int type
-:8:5: error: expected record type for field selection, got unit type
-:9:5: error: record { bar : string, fo : int }#0 type has no field named foo
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
-:8:3: error: undefined variable: a
-:9:8: error: expected int type for right-hand side of assignment, got string type
-:10:8: error: expected record { foo : int }#0 type for right-hand side of assignment, got record { foo : int }#1 type
-:11:22: error: variable cannot be assigned to: i
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
-:4:12: error: expected 3 fields, got 0 fields
-:6:3: error: undefined record type: r
-:7:3: error: expected record type for record creation, got int type
-:8:3: error: expected 3 fields, got 1 fields
-:8:9: error: expected field 'foo' of int type, got field 'foo' of unit type
-:9:9: error: expected field 'foo' of int type, got field 'foo' of string type
-:10:16: error: expected field 'bar' of string type, got field 'baz' of nil type
-:10:25: error: expected field 'baz' of record { foo : int, bar : string, baz : alias 'rec' }#0 type, got field 'bar' of string type
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
-:4:13: error: undefined array type: z
-:5:13: error: expected int type for initial value of array, got string type
-:7:6: error: expected int type for array size, got unit type
-:8:3: error: expected array type for array creation, got int type
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
-:2:2: error: expected int type for condition of if expression, got string type
-:2:2: error: incompatible branch types for if expression
-:3:2: error: incompatible branch types for if expression
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
-:8:17: error: cannot call non-function variable: f
-:8:13: error: expected 1 parameters, got 2 arguments
-:8:30: error: expected record { bar : int }#0 type for argument 1, got record { bar : int }#1 type
-:8:49: error: undefined function: bar
|});

    "while loop" >:: (fun _ ->
      assert_ok
        {|let var i := 2 in while i < 10 do i := i + 1 end|}
        Type.UnitType);

    "while loop errors" >:: (fun _ ->
      assert_error
        {|while "a" do 30|}
        {|
-:1:1: error: expected int type for condition of while loop, got string type
-:1:1: error: expected unit type for body of while loop, got int type
|});

    "for loop" >:: (fun _ ->
      assert_ok
        {|for i := 1 to 10 do print("")|}
        Type.UnitType);

    "for loop errors" >:: (fun _ ->
      assert_error
        {|for abc := "" to () do 328|}
        {|
-:1:1: error: expected int type for lower bound of for loop, got string type
-:1:1: error: expected int type for higher bound of for loop, got unit type
-:1:1: error: expected unit type for body of for loop, got int type
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
-:3:20: error: break expression must be inside loop
-:6:1: error: break expression must be inside loop
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
-:3:23: error: duplicate field: a
-:7:3: error: duplicate type declaration: a
-:7:3: error: type alias cycle detected: b, d, c
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
-:5:11: error: undefined type: integer
-:6:3: error: variable declaration must specify type for nil initial value
-:7:3: error: expected string type for variable initialization, got int type
-:8:3: error: expected (array of int)#0 type for variable initialization, got (array of int)#1 type
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
-:4:3: error: duplicate function declaration: foo
-:5:25: error: duplicate paramater: x
-:5:3: error: expected int type for function body, got nil type
-:7:7: error: expected string type for argument 1, got int type
|})
    ]
  ]

let () =
  run_test_tt_main test_typecheck