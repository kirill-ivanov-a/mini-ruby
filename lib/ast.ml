(* AST *)

type identifier = string [@@deriving show {with_path= false}]
type boolop = And | Or [@@deriving show {with_path= false}]

type arithop = Add | Sub | Mul | Div | Mod | Pow
[@@deriving show {with_path= false}]

type unaryop = Inv | Not [@@deriving show {with_path= false}]

type cmpop = Eq | NotEq | Lt | LtE | Gt | GtE
[@@deriving show {with_path= false}]

type number = Int of int | Float of float [@@deriving show {with_path= false}]
type modifier = Local | Instance [@@deriving show {with_path= false}]

type str = Literal of string | Substitution of expr
[@@deriving show {with_path= false}]

and args = expr list [@@deriving show {with_path= false}]

and params = identifier list [@@deriving show {with_path= false}]

and assoc_var = AssocVar of var | Self | ClassName of identifier
[@@deriving show {with_path= false}]

and values = expr list [@@deriving show {with_path= false}]

and lvalues = var list [@@deriving show {with_path= false}]

and var = VarName of modifier * identifier | ListItem of var * expr
[@@deriving show {with_path= false}]

and proc = Lambda of params * stmt list [@@deriving show {with_path= false}]

and expr =
  | ArithOp of arithop * expr * expr
  | BoolOp of boolop * expr * expr
  | CmpOp of cmpop * expr * expr
  | UnaryOp of unaryop * expr
  | Number of number
  | String of str list
  (* example: "str#{i}ng" -> [Expr (String [Literal "str"; Substitution (Var (Local, "i")); Literal "ng"])]*)
  | ProcCall of proc * args (* args *)
  | MethodCall of identifier (* method name *) * args (* args *)
  (* the object must be found in the environment *)
  | AssocMethodCall of assoc_var * identifier * args
  | List of values
  | Proc of proc
  | Var of var
  | True
  | False
  | Nil
[@@deriving show {with_path= false}]

and stmt =
  | ClassDef of identifier * stmt list
  | AssocMethodDef of assoc_var * identifier * params * stmt list (* definition of a method associated with a variable *)
  | MethodDef of identifier * params * stmt list (* instance method definition *)
  | Return of values
  | Assign of lvalues * values (* v0, v1, ..., vn = exp0, ... *)
  | While of expr * stmt list
  | Until of expr * stmt list
  | If of expr * stmt list * stmt list (* else *)
  | Expr of expr
  | Break
  (* 'continue' in ruby *)
  | Next
[@@deriving show {with_path= false}]
