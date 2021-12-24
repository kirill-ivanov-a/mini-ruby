open Format
open Ast

let string_of_arithop = function
  | Add -> "+"
  | Sub -> "-"
  | Div -> "/"
  | Mul -> "*"
  | Mod -> "%"
  | Pow -> "**"

let string_of_boolop = function And -> "and" | Or -> "or"

let string_of_cmpop = function
  | Eq -> "=="
  | NotEq -> "!="
  | Lt -> "<"
  | LtE -> "<="
  | Gt -> ">"
  | GtE -> ">="

let string_of_unop = function Inv -> "-" | Not -> "not "
let string_of_modifier = function Local -> "" | Instance -> "@"
let pp_parens pp fmt = fprintf fmt "(%a)" pp
let pp_char = pp_print_char
let pp_string = pp_print_string
let pp_int = pp_print_int
let pp_float = pp_print_float

let pp_list pp fmt =
  let sep fmt () = fprintf fmt ", " in
  pp_print_list ~pp_sep:sep pp fmt

let rec pp_proc fmt (params, body) =
  fprintf fmt "->(%a){%a}" (pp_list pp_string) params pp_body body

and pp_stmts fmt =
  let sep fmt () = fprintf fmt "@;<0 0>@;<0 0>" in
  pp_print_list ~pp_sep:sep pp_stmt fmt

and pp_body fmt =
  let sep fmt () = fprintf fmt "@;<0 0>" in
  pp_print_list ~pp_sep:sep pp_stmt fmt

and pp_var fmt = function
  | ListItem (var, index) -> fprintf fmt "%a[%a]" pp_var var pp_expr index
  | VarName (modifier, name) ->
      fprintf fmt "%s%s" (string_of_modifier modifier) name

and pp_assoc_var fmt = function
  | AssocVar v -> pp_var fmt v
  | Self -> pp_string fmt "self"
  | ClassName name -> fprintf fmt "%s" name

and pp_stmt fmt = function
  | ClassDef (name, body) ->
      fprintf fmt "@[<v 0>class %s@;<0 2>@[<v 0>%a@]@;<0 0>end@]" name pp_stmts
        body
  | MethodDef (method_name, params, stmts) ->
      fprintf fmt "@[<v 0>def %s(%a)@;<0 2>@[<v 0>%a@]@;<0 0>end@]" method_name
        (pp_list pp_string) params pp_body stmts
  | AssocMethodDef (assoc_var, method_name, params, stmts) ->
      fprintf fmt "@[<v 0>def %a.%s(%a)@;<0 2>@[<v 0>%a@]@;<0 0>end@]"
        pp_assoc_var assoc_var method_name (pp_list pp_string) params pp_body
        stmts
  | Return values -> fprintf fmt "@[<v 0>return %a@]" (pp_list pp_expr) values
  | While (cond, body) ->
      fprintf fmt "@[<v 0>while %a@;<0 2>@[<v 0>%a@]@;<0 0>end@]" pp_expr cond
        pp_body body
  | Until (cond, body) ->
      fprintf fmt "@[<v 0>until %a@;<0 2>@[<v 0>%a@]@;<0 0>end@]" pp_expr cond
        pp_body body
  | Assign (tragets, values) ->
      fprintf fmt "@[<v 0>%a = %a@]" (pp_list pp_var) tragets (pp_list pp_expr)
        values
  | If (cond, if_stmts, else_stmts) ->
      let pp_else fmt = function
        | [] -> ()
        | stmts -> fprintf fmt "@[<v 0>else@;<0 2>@[<v 0>%a@]@]" pp_body stmts
      in
      fprintf fmt "@[<v 0>if %a@;<0 2>@[<v 0>%a@]@;<0 0>%a@;<0 0>end@]" pp_expr
        cond pp_body if_stmts pp_else else_stmts
  | Expr expr -> pp_expr fmt expr
  | Break -> pp_string fmt "break"
  | Next -> pp_string fmt "next"

and pp_expr fmt =
  let pp_parens_expr = pp_parens pp_expr in
  function
  | BoolOp (op, l, r) ->
      let get_pp =
        let check_and = function
          | BoolOp (Or, _, _), BoolOp (Or, _, _) ->
              (pp_parens_expr, pp_parens_expr)
          | BoolOp (Or, _, _), _ -> (pp_parens_expr, pp_expr)
          | _, BoolOp (Or, _, _) -> (pp_expr, pp_parens_expr)
          | _, _ -> (pp_expr, pp_expr) in
        function And, l, r -> check_and (l, r) | _, _, _ -> (pp_expr, pp_expr)
      in
      let pair = get_pp (op, l, r) in
      fprintf fmt "%a %s %a" (fst pair) l (string_of_boolop op) (snd pair) r
  | ArithOp (op, l, r) ->
      let get_pp =
        let check_factop = function
          | ArithOp ((Add | Sub), _, _), ArithOp ((Add | Sub), _, _) ->
              (pp_parens_expr, pp_parens_expr)
          | ArithOp ((Add | Sub), _, _), _ -> (pp_parens_expr, pp_expr)
          | _, ArithOp ((Add | Sub), _, _) -> (pp_expr, pp_parens_expr)
          | _, _ -> (pp_expr, pp_expr) in
        let check_pow = function
          | ( ArithOp ((Add | Sub | Mul | Div | Mod), _, _)
            , ArithOp ((Add | Sub | Mul | Div | Mod), _, _) ) ->
              (pp_parens_expr, pp_parens_expr)
          | ArithOp ((Add | Sub | Mul | Div | Mod), _, _), _ ->
              (pp_parens_expr, pp_expr)
          | _, ArithOp ((Add | Sub | Mul | Div | Mod), _, _) ->
              (pp_expr, pp_parens_expr)
          | _, _ -> (pp_expr, pp_expr) in
        function
        | (Mul | Div | Mod), l, r -> check_factop (l, r)
        | Pow, l, r -> check_pow (l, r)
        | _ -> (pp_expr, pp_expr) in
      let pair = get_pp (op, l, r) in
      fprintf fmt "%a %s %a" (fst pair) l (string_of_arithop op) (snd pair) r
  | CmpOp (op, l, r) ->
      fprintf fmt "%a %s %a" pp_expr l (string_of_cmpop op) pp_expr r
  | UnaryOp (op, expr) ->
      let get_pp = function
        | BoolOp _ | CmpOp _ | ArithOp _ -> pp_parens_expr
        | _ -> pp_expr in
      fprintf fmt "%s%a" (string_of_unop op) (get_pp expr) expr
  | String parts ->
      let sep fmt () = pp_string fmt "" in
      let pp_string_part fmt = function
        | Substitution expr -> fprintf fmt "#{%a}" pp_expr expr
        | Literal l -> pp_string fmt l in
      fprintf fmt "\"%a\"" (pp_print_list ~pp_sep:sep pp_string_part) parts
  | ProcCall (Lambda (params, body), args) ->
      fprintf fmt "%a.call(%a)" pp_proc (params, body) (pp_list pp_expr) args
  | MethodCall (method_name, args) ->
      fprintf fmt "%s(%a)" method_name (pp_list pp_expr) args
  | AssocMethodCall (assoc_var, method_name, args) ->
      fprintf fmt "%a.%s(%a)" pp_assoc_var assoc_var method_name
        (pp_list pp_expr) args
  | List values -> fprintf fmt "[%a]" (pp_list pp_expr) values
  | Proc (Lambda (params, stmts)) -> pp_proc fmt (params, stmts)
  | Number (Int n) -> pp_int fmt n
  | Number (Float f) -> pp_float fmt f
  | Var v -> pp_var fmt v
  | True -> pp_string fmt "true"
  | False -> pp_string fmt "false"
  | Nil -> pp_string fmt "nil"

let print_prog fmt stmts = fprintf fmt "@[<v 0>%a@]" pp_stmts stmts
