%{ open Syntax %}

%start <Syntax.expr> parse_expr
%start <Syntax.statement> parse_statement
%start <Syntax.type_> parse_type
%start <Syntax.top_level_item> parse_top_level_item
%start <Syntax.top_level> parse_top_level

%token LBR RBR LBK RBK LPR RPR COLON COMMA SEMI EQ PLUS MINUS NOT TWIDDLE DOT
       MUL DIV GE LE LT GT EQL NEQ AND OR AT

%token PACKAGE CLASS INTERFACE PRIMITIVE EXTENDS RETURN MACRO NEW NULL IF ELSE
       WHILE VAR

%token <string> ID STRING
%token <int> NUMBER

%token EOF

(* Associatifity and precedence, from low to high *)
%left OR
%left AND
%left GE LE LT GT EQL NEQ
%left PLUS MINUS
%left MUL DIV
%left UNARY_PRECEDENCE
%left LPR
%left DOT

%%

(* Entry points *)
parse_expr: expr EOF { $1 }
parse_statement: statement EOF { $1 }
parse_type: type_ EOF { $1 }
parse_top_level_item: top_level_item EOF { $1 }
parse_top_level: top_level EOF { $1 }

top_level: annotated(top_level_item)* { $1 }

annotation: AT name=ID arguments=loption(arguments) { name, arguments }
annotated(ITEM): items=pair(annotation*, ITEM) { items }

top_level_item:
| function_ { PackageItem $1 }
| class_ { PackageItem $1 }
| package { PackageItem $1 }
| macro { TopLevelMacro $1 }

function_: signature=signature body=block { Function (signature, body) }

class_: hierarchy=hierarchy name=ID parameters=loption(type_parameters)
        super=preceded(EXTENDS, ID)? body=braced(annotated(class_item)*)
    { Hierarchy (hierarchy, name, parameters, super, body) }

package: PACKAGE name=ID items=braced(annotated(package_item)*)
    { Package (name, items) }

package_item:
| package { $1 }
| class_ { $1 }
| function_ { $1 }

class_item:
| signature SEMI { Prototype $1  }
| signature=signature body=block { Method (signature, body) }
| var { Field $1 }
| name=ID parameters=parameters body=block
    { Constructor (name, parameters, body) }
| macro { ClassMacro $1 }

macro: MACRO signature=signature e=expr SEMI { signature, e }

signature: type_=type_ name=ID parameters=parameters
    { type_, name, parameters }

hierarchy:
| CLASS { Class }
| INTERFACE { Interface }
| PRIMITIVE { Primitive }

type_: path=path parameters=loption(type_parameters)
    { Type (path, parameters) }
type_parameters: chevroned(comma_separated(type_)) { $1 }
path: separated_nonempty_list(DOT, ID) { $1 }

var: type_=type_ name=ID initial=preceded(EQ, expr)? SEMI
    { type_, name, initial }

statement:
| RETURN e=expr SEMI { Return e }
| IF condition=condition consequence=block { If (condition, consequence) }
| IF condition=condition consequence=block ELSE alternative=block
    { IfElse (condition, consequence, alternative) }
| WHILE condition=condition body=block { While (condition, body) }
| e=expr SEMI { Expr e }
| VAR var=var { Local var }
| left=lvalue EQ right=expr SEMI { Assignment (left, right) }

parameter: type_=type_ name=ID default=preceded(EQ, expr)?
    { {type_; name; default} }
parameters: parenthesised(comma_separated(parameter)) { $1 }
condition: parenthesised(expr) { $1 }
block: braced(statement*) { $1 }

expr:
| STRING { String $1 }
| NUMBER { Number $1 }
| NULL { Null }
| bracketed(comma_separated(expr)) { List $1 }
| braced(comma_separated(separated_pair(expr, COLON, expr))) { Map $1 }
| left=expr op=infix_operator right=expr { Infix (left, op, right) }
| op=unary_operator e=expr %prec UNARY_PRECEDENCE { Unary (op, e) }
| e=expr arguments=arguments { Call (e, arguments) }
| e=parenthesised(expr) { e }
| e=lvalue { e }
| NEW type_=type_ arguments=arguments { New (type_, arguments) }

arguments: parenthesised(comma_separated(expr)) { $1 }

lvalue:
| ID { Identifier $1 }
| e=expr DOT attribute=ID { AttributeAccess (e, attribute) }

%inline unary_operator:
| NOT { Not }
| TWIDDLE { Twiddle }
| MINUS { Negated }

%inline infix_operator:
| OR { Or }
| AND { And }
| PLUS { Plus }
| MINUS { Minus }
| MUL { Mul }
| DIV { Div }
| GE { Ge }
| LE { Le }
| LT { Lt }
| GT { Gt }
| EQL { Eql }
| NEQ { Neq }

parenthesised(BODY): LPR body=BODY RPR { body }
bracketed(BODY):     LBK body=BODY RBK { body }
chevroned(BODY):     LT  body=BODY GT { body }
braced(BODY):        LBR body=BODY RBR { body }

comma_separated(ITEM): items=separated_list(COMMA, ITEM) { items }
