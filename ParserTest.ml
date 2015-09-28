open Syntax
let (test), (=>) = Test.(test, (=>))

let parse_with parser_function source =
  let lexbuf = Lexing.from_string source in
  parser_function Lexer.read lexbuf

let expr = parse_with Parser.parse_expr
let statement = parse_with Parser.parse_statement
let type_ = parse_with Parser.parse_type
let item = parse_with Parser.parse_top_level_item
let top = parse_with Parser.parse_top_level

let a, b, c = Identifier "a", Identifier "b", Identifier "c"

module TestExprs = struct
  let () = test "identifier" @@ fun () ->
    expr "foo" => Identifier "foo"

  let () = test "literals" @@ fun () ->
    expr "\"hai\"" => String "hai";
    expr "123" => Number 123;
    expr "null" => Null;
    expr "[123, null, \"hai\"]" => List [Number 123; Null; String "hai"];
    expr "{123: null, \"hai\": 456}" => Map [Number 123, Null;
                                             String "hai", Number 456]

  let () = test "infix & precedence" @@ fun () ->
    expr "a || b && c" => Infix (a, Or, Infix (b, And, c));
    expr "a && b || c" => Infix (Infix (a, And, b), Or, c);
    expr "a + b * c" => Infix (a, Plus, Infix (b, Mul, c));
    expr "a * b + c" => Infix (Infix (a, Mul, b), Plus, c)

  let () = test "unary" @@ fun () ->
    expr "!a" => Unary (Not, a);
    expr "~a" => Unary (Twiddle, a);
    expr "-a" => Unary (Negated, a)

  let () = test "attribute" @@ fun () ->
    expr "foo.bar" => AttributeAccess (Identifier "foo", "bar");
    expr "123.bar" => AttributeAccess (Number 123, "bar")

  let () = test "call" @@ fun () ->
    expr "foo()" => Call (Identifier "foo", []);
    expr "foo(a, b, c)" => Call (Identifier "foo", [a; b; c])

  let () = test "grouping with parenthesis" @@ fun () ->
    expr "(a + b) * c" => Infix (Infix (a, Plus, b), Mul, c);
    expr "a * (b + c)" => Infix (a, Mul, Infix (b, Plus, c))

  let () = test "new" @@ fun () ->
    expr "new Foo<Bar>(a, b, c)" => New (Type (["Foo"], [Type (["Bar"], [])]),
                                         [a; b; c])
end

module TestStatements = struct
  let () = test "return" @@ fun () ->
    statement "return a;" => Return a

  let () = test "if else" @@ fun () ->
    statement "if (a) { return a; }" => If (a, [Return a]);
    statement "if (a) { return a; } else { return b; }"
      => IfElse (a, [Return a], [Return b])

  let () = test "while" @@ fun () ->
    statement "while (a) { return b; }" => While (a, [Return b])

  let () = test "expr statement" @@ fun () ->
    statement "a < b;" => Expr (Infix (a, Lt, b));
    statement "a < b > c;" => Expr (Infix (Infix (a, Lt, b), Gt, c))

  let () = test "local variable" @@ fun () ->
    statement "var foo a;" => Local (Type (["foo"], []), "a", None);
    statement "var foo a = b;" => Local (Type (["foo"], []), "a", Some b)

  let () = test "assignment" @@ fun () ->
    statement "a.bar = c;"
      => Assignment (AttributeAccess (a, "bar"), c);
    statement "a().bar = c;"
      => Assignment (AttributeAccess (Call (a, []), "bar"), c)
end

module TestTypes = struct
  let () = test "type" @@ fun () ->
    type_ "foo" => Type (["foo"], []);
    type_ "foo.bar" => Type (["foo"; "bar"], []);
    type_ "foo.bar<>" => Type (["foo"; "bar"], []);
    type_ "foo.bar<baz>" => Type (["foo"; "bar"], [Type (["baz"], [])]);
    type_ "foo.bar<baz<qux>>"
      => Type (["foo"; "bar"], [Type (["baz"], [Type (["qux"], [])])]);
end

let type_ name = Type ([name], [])

module TestTopLevelItems = struct
  let () = test "function" @@ fun () ->
    item "int f(bool b=true, char c) { return a; }"
      => PackageItem (Function ((type_ "int", "f", [
        {type_=type_ "bool"; name="b"; default=Some (Identifier "true")};
        {type_=type_ "char"; name="c"; default=None};
      ]), [Return a]))

  let () = test "class" @@ fun () ->
    item "class Foo<T, U> extends Bar {
            int field = a;
            constructor() { return a; }
            int prototype();
            int method() { return a; }
            macro int MACRO() a;
          }"
      => PackageItem (Hierarchy (Class, "Foo", [type_ "T";
                                                   type_ "U"], Some "Bar", [
           [], Field (type_ "int", "field", Some a);
           [], Constructor ("constructor", [], [Return a]);
           [], Prototype (type_ "int", "prototype", []);
           [], Method ((type_ "int", "method", []), [Return a]);
           [], ClassMacro ((type_ "int", "MACRO", []), a);
         ]))

  let () = test "class" @@ fun () ->
    item "package foo {
            package bar { }
            int function() { return a; }
            class baz { }
          }"
      => PackageItem (Package ("foo", [
           [], Package ("bar", []);
           [], Function ((type_ "int", "function", []), [Return a]);
           [], Hierarchy (Class, "baz", [], None, []);
         ]))
end

module TestTopLevel = struct
  let () = test "top_level" @@ fun () ->
    top "package foo { }
         macro int bar() a;"
      => [
        [], PackageItem (Package ("foo", []));
        [], TopLevelMacro ((type_ "int", "bar", []), a);
      ];
    top "@qux package foo { }
         @mux(b, c) @lux macro int bar() a;"
      => [
        ["qux", []],
          PackageItem (Package ("foo", []));
        ["mux", [b; c]; "lux", []],
          TopLevelMacro ((type_ "int", "bar", []), a);
      ]
end
