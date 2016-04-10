type type_ = Type of string list * type_ list
  [@@deriving to_yojson]

type infix =
  And | Or | Plus | Minus | Mul | Div | Ge | Le | Lt | Gt | Eql | Neq
  [@@deriving to_yojson]

type unary = Not | Twiddle | Negated
  [@@deriving to_yojson]

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
  [@@deriving to_yojson]

type binding = {type_: type_; name: string; value: expr option}
  [@@deriving to_yojson]

type parameter = binding
  [@@deriving to_yojson]

type annotation = string * expr list
  [@@deriving to_yojson]

type 'a annotated = {annotations: annotation list; item: 'a}
  [@@deriving to_yojson]

type variable = binding
  [@@deriving to_yojson]

type statement =
  | Return of expr
  | If of expr * statement list
  | IfElse of expr * statement list * statement list
  | While of expr * statement list
  | Expr of expr
  | Local of variable
  | Assignment of expr * expr
  [@@deriving to_yojson]

type signature = Signature of type_ * string * parameter list
  [@@deriving to_yojson]

type hierarchy = Class | Interface | Primitive
  [@@deriving to_yojson]

type macro = signature * expr
  [@@deriving to_yojson]

type class_item =
  | Prototype of signature
  | Method of signature * statement list
  | StaticMethod of signature * statement list
  | Field of variable
  | StaticField of variable
  | Constructor of string * parameter list * statement list
  | ClassMacro of macro
  [@@deriving to_yojson]

type namespace_item =
  | Namespace of string * namespace_item annotated list
  | Function of signature * statement list
  | Hierarchy of hierarchy * string * type_ list * string option
                * class_item annotated list
  | Macro of macro
  [@@deriving to_yojson]

type top_level_item =
  | Use of string
  | Include of string
  | Package of string * string
  | NamespaceItem of namespace_item
  [@@deriving to_yojson]

type top_level = TopLevel of top_level_item annotated list
  [@@deriving to_yojson]

let to_json = top_level_to_yojson
