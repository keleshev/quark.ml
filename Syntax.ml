type type_ = Type of string list * type_ list

type infix = And | Or | Plus | Minus | Mul | Div | Ge | Le | Lt | Gt | Eql | Neq
type unary = Not | Twiddle | Negated

type expr =
  | String of string
  | Number of int
  | Null
  | List of expr list
  | Map of (expr * expr) list
  | Infix of expr * infix * expr
  | Unary of unary * expr
  | Call of expr * expr list
  | Identifier of string
  | AttributeAccess of expr * string
  | New of type_ * expr list

type parameter = {type_: type_; name: string; default: expr option}

type annotation = string * expr list
type 'a annotated = annotation list * 'a

type variable = type_ * string * expr option

type statement =
  | Return of expr
  | If of expr * statement list
  | IfElse of expr * statement list * statement list
  | While of expr * statement list
  | Expr of expr
  | Local of variable
  | Assignment of expr * expr

type signature = type_ * string * parameter list

type hierarchy = Class | Interface | Primitive

type macro = signature * expr

type class_item =
  | Prototype of signature
  | Method of signature * statement list
  | Field of variable
  | Constructor of string * parameter list * statement list
  | ClassMacro of macro

type package_item =
  | Package of string * package_item annotated list
  | Function of signature * statement list
  | Hierarchy of hierarchy * string * type_ list * string option
                * class_item annotated list

type top_level_item = PackageItem of package_item | TopLevelMacro of macro

type top_level = top_level_item annotated list
