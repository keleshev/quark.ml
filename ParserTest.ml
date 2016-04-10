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
    statement "null.bar;" => Expr (AttributeAccess (Null, "bar"));
    statement "(a < b);" => Expr (Infix (a, Lt, b));
    statement "(a < b > c);" => Expr (Infix (Infix (a, Lt, b), Gt, c));
    statement "null();" => Expr (Call (Null, []));
    statement "a();" => Expr (Call (a, []));
    statement "a.bar;" => Expr (AttributeAccess (a, "bar"))

  let () = test "local variable" @@ fun () ->
    statement "foo a;" =>
      Local {type_=Type (["foo"], []); name="a"; value=None};
    statement "foo a = b;" =>
      Local {type_=Type (["foo"], []); name="a"; value=Some b};
    statement "a < b > c;" =>
      Local {type_=Type (["a"], [Type (["b"], [])]); name="c"; value=None}

  let () = test "assignment" @@ fun () ->
    statement "a = c;"
      => Assignment (a, c);
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
      => NamespaceItem (Function (Signature (type_ "int", "f", [
        {type_=type_ "bool"; name="b"; value=Some (Identifier "true")};
        {type_=type_ "char"; name="c"; value=None};
      ]), [Return a]))

  let () = test "class" @@ fun () ->
    item "class Foo<T, U> extends Bar {
            int field = a;
            constructor() { return a; }
            int prototype();
            int method() { return a; }
            macro int MACRO() a;
          }"
      => NamespaceItem (Hierarchy (Class, "Foo", [type_ "T";
                                                type_ "U"], Some "Bar", [
           {annotations=[];
            item=Field {type_=type_ "int"; name="field"; value=Some a}};
           {annotations=[];
            item=Constructor ("constructor", [], [Return a])};
           {annotations=[];
            item=Prototype (Signature (type_ "int", "prototype", []))};
           {annotations=[];
           item=Method (Signature (type_ "int", "method", []), [Return a])};
           {annotations=[];
            item=ClassMacro (Signature (type_ "int", "MACRO", []), a)};
         ]))

  let () = test "namespace" @@ fun () ->
    item "package foo {}" => NamespaceItem (Namespace ("foo", []));
    item "namespace foo {
            namespace bar { }
            int function() { return a; }
            class baz { }
            macro int MACRO() a;
          }"
      => NamespaceItem (Namespace ("foo", [
           {annotations=[];
            item=Namespace ("bar", [])};
           {annotations=[];
            item=Function (Signature (type_ "int", "function", []),
                           [Return a])};
           {annotations=[];
            item=Hierarchy (Class, "baz", [], None, [])};
           {annotations=[];
            item=Macro (Signature (type_ "int", "MACRO", []), a)};
         ]))

  let () = test "package" @@ fun () ->
    item "package foo 1.2.3-ab.cd+12.34;"
      => Package ("foo", "1.2.3-ab.cd+12.34")

  let () = test "use/include" @@ fun () ->
    item "use http://foo.bar;" => Use "http://foo.bar";
    item "include http://foo.bar;" => Include "http://foo.bar"
end

module TestTopLevel = struct
  let () = test "top_level" @@ fun () ->
    top "namespace foo { }
         macro int bar() a;"
      => TopLevel [
        {annotations=[];
         item=NamespaceItem (Namespace ("foo", []))};
        {annotations=[];
         item=NamespaceItem (Macro (Signature (type_ "int", "bar", []), a))};
      ];
    top "@qux namespace foo { }
         @mux(b, c) @lux macro int bar() a;"
      => TopLevel [
        {annotations=["qux", []];
         item=NamespaceItem (Namespace ("foo", []))};
        {annotations=["mux", [b; c]; "lux", []];
         item=NamespaceItem (Macro (Signature (type_ "int", "bar", []), a))};
      ]
end
