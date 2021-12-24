open Angstrom
open Ast

let parse p s = parse_string ~consume:All p s

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let rec chainr1 e op =
  e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_ws = function ' ' | '\t' -> true | _ -> false
let is_eol = function '\r' | '\n' -> true | _ -> false
let is_semicolon = function ';' -> true | _ -> false
let is_sep c = is_semicolon c || is_eol c
let is_space c = is_sep c || is_ws c
let is_digit = function '0' .. '9' -> true | _ -> false

let is_keyword = function
  | "__ENCODING__" | "__LINE__" | "__FILE__" | "BEGIN" | "END" | "alias"
   |"and" | "begin" | "break" | "case" | "class" | "def" | "do" | "else"
   |"elsif" | "end" | "ensure" | "false" | "for" | "if" | "in" | "module"
   |"next" | "nil" | "not" | "return" | "self" | "super" | "or" | "then"
   |"true" | "undef" | "unless" | "until" | "when" | "while" | "yield"
   |"retry" | "redo" | "resque" | "lambda" ->
      true
  | _ -> false

let ws = take_while is_ws
let del_ws p = ws *> p <* ws
let eol = take_while is_eol
let space = take_while is_space
let space1 = take_while1 is_space
let ws1 = take_while1 is_ws
let seps = take_while is_sep
let seps1 = take_while1 is_sep
let token s = ws *> string s
let left_of p1 p = p <* space <* p1
let right_of p1 p = p1 *> space *> p
let between p1 p2 p = left_of p2 (right_of p1 p)

(* braces *)
let rcb = token "}"
let lcb = token "{"
let rsb = token "]"
let lsb = token "["
let rp = token ")"
let lp = token "("
let parens p = between lp rp p

(* tokens *)
let _then = token "then"
let _else = token "else"
let _end = eol *> token "end"
let _do = token "do"
let _return = token "return"
let _if = token "if"
let _elsif = token "elsif"
let _else = token "else"
let _class = token "class"
let _def = token "def"
let _while = token "while"
let _until = token "until"
let _lambda = token "lambda"
let _semi = token ";"
let _eol = token "\n"
let _comma = token "," <* space
let _dot = token "." <* space
let _self = token "self"

(* Constructors *)
module Ctors = struct
  let unop op value = UnaryOp (op, value)
  let arithop op e1 e2 = ArithOp (op, e1, e2)
  let boolop bop e1 e2 = BoolOp (bop, e1, e2)
  let cmpop cmp e1 e2 = CmpOp (cmp, e1, e2)
  let assoc_method_def var id args stmts = AssocMethodDef (var, id, args, stmts)
  let method_def id args stmts = MethodDef (id, args, stmts)
  let class_def id stmts = ClassDef (id, stmts)
  let while_stmt expr stmts = While (expr, stmts)
  let until_stmt expr stmts = Until (expr, stmts)
  let if_stmt expr stmts1 stmts2 = If (expr, stmts1, stmts2)
  let assign targets values = Assign (targets, values)
  let lambda args stmts = Lambda (args, stmts)
  let proc_call proc args = ProcCall (proc, args)
  let list itms = List itms
  let assoc_method_call obj id args = AssocMethodCall (obj, id, args)
  let cls name = ClassName name
  let list_item v idx = ListItem (v, idx)
  let method_call id args = MethodCall (id, args)
  let expr_stmt expr = Expr expr
  let return values = Return values
  let var v = Var v
  let assoc_var v = AssocVar v
  let proc p = Proc p
end

(* operators *)
let _add = many1 (token "+") *> return (Ctors.arithop Add)
let _sub = token "-" *> return (Ctors.arithop Sub)
let _mul = token "*" *> return (Ctors.arithop Mul)
let _div = token "/" *> return (Ctors.arithop Div)
let _mod = token "%" *> return (Ctors.arithop Mod)
let _pow = token "**" *> return (Ctors.arithop Pow)
let _inv = token "-" *> return (Ctors.unop Inv)
let _not = token "not" *> return (Ctors.unop Not)
let _and = token "and" *> return (Ctors.boolop And)
let _or = token "or" *> return (Ctors.boolop Or)
let _eq = token "==" *> return (Ctors.cmpop Eq)
let _neq = token "!=" *> return (Ctors.cmpop NotEq)
let _lt = token "<" *> return (Ctors.cmpop Lt)
let _gt = token ">" *> return (Ctors.cmpop Gt)
let _lte = token "<=" *> return (Ctors.cmpop LtE)
let _gte = token ">=" *> return (Ctors.cmpop GtE)
let _break = token "break" *> return Break
let _next = token "next" *> return Next

(* constants and variables *)
let _true = string "true" *> return True
let _false = string "false" *> return False
let _nil = string "nil" *> return Nil

let identifier is_valid_first_char =
  let is_valid_id_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false in
  ws *> peek_char
  >>= function
  | Some c when is_valid_first_char c -> (
      take_while is_valid_id_char
      >>= fun id ->
      match is_keyword id with false -> return id | true -> fail "Invalid id" )
  | _ -> fail "Invalid id"

let id =
  let is_valid_first_char = function 'a' .. 'z' | '_' -> true | _ -> false in
  identifier is_valid_first_char

let method_id =
  id
  >>= fun s ->
  peek_char
  >>= function Some '=' -> advance 1 >>| fun () -> s ^ "=" | _ -> return s

let class_id =
  let is_valid_first_char = function 'A' .. 'Z' | '_' -> true | _ -> false in
  identifier is_valid_first_char

let inst_var = char '@' *> id >>= fun s -> return @@ VarName (Instance, s)
let local_var = id >>= fun s -> return @@ VarName (Local, s)
let cls = class_id >>| Ctors.cls
let self = _self *> return Self

let var_name =
  del_ws (peek_char_fail >>= function '@' -> inst_var | _ -> local_var)

let var = var_name >>| Ctors.var
let assoc_var = var_name >>| Ctors.assoc_var <|> self <|> cls

let number =
  let integer = take_while1 is_digit in
  let sign =
    peek_char
    >>= function
    | Some '-' -> advance 1 >>| fun () -> "-"
    | Some '+' -> advance 1 >>| fun () -> "+"
    | Some c when is_digit c -> return ""
    | _ -> fail "Sign or digit expected" in
  let dot =
    peek_char
    >>= function Some '.' -> advance 1 >>| fun () -> true | _ -> return false
  in
  ws *> sign
  >>= fun sign ->
  integer
  >>= fun whole ->
  dot
  >>= function
  | false -> return (Number (Int (int_of_string (sign ^ whole))))
  | true ->
      take_while1 is_digit <* ws
      >>= fun part ->
      return (Number (Float (float_of_string (sign ^ whole ^ "." ^ part))))

let factop = _mul <|> _div <|> _mod <?> "'*' or '/' or '%' expected" <* space
let termop = ws *> _add <|> _sub <?> "'+' or '-' expected" <* space

let cmpop =
  _lte <|> _lt <|> _gte <|> _gt <|> _neq <|> _eq <?> "compare operator expected"
  <* space

let rec dup_exist = function
  | [] -> false
  | hd :: tl -> List.exists (( = ) hd) tl || dup_exist tl

let params =
  sep_by _comma id
  >>= fun ps ->
  if dup_exist ps then fail "duplicates in parameter names" else return ps

let prog =
  fix (fun prog ->
      let expr =
        fix (fun expr ->
            let list =
              let items = sep_by _comma expr in
              between lsb rsb items >>| Ctors.list in
            let str =
              let subs =
                char '#' *> between (char '{') (char '}') expr
                >>= fun e -> return (Substitution e) in
              let lit =
                take_while1 (function '\"' | '#' -> false | _ -> true)
                >>= fun s -> return @@ Literal s in
              let hash = string "#" >>= fun s -> return @@ Literal s in
              char '\"' *> many (choice [lit; subs; hash])
              >>= fun s -> return @@ Ast.String s <* char '\"' in
            let const =
              ws *> peek_char_fail
              >>= function
              | 'f' -> _false
              | 't' -> _true
              | 'n' -> _nil
              | '\"' -> str
              | _ -> number in
            let empty : expr list = [] in
            let args = sep_by _comma expr in
            let vb = token "|" in
            let lambda_args = between vb vb args in
            let lambda_params = between vb vb params in
            let list_item =
              let index = between lsb rsb expr in
              let rec take_index idx ctor =
                idx
                >>= fun a ->
                take_index idx (Ctors.list_item @@ ctor a) <|> return (ctor a)
              in
              var_name >>= fun name -> take_index index (Ctors.list_item name)
            in
            let list_item_expr = list_item >>| Ctors.var in
            let assoc_method_call =
              let assoc = list_item >>| Ctors.assoc_var <|> assoc_var <* _dot in
              lift3 Ctors.assoc_method_call assoc method_id (parens args) in
            let method_call = lift2 Ctors.method_call method_id (parens args) in
            let inverse =
              fix (fun inv ->
                  let minus = token "-" <* space in
                  let value = choice [inv; expr] in
                  _inv <* space <*> value) in
            let other =
              choice [assoc_method_call; method_call; list_item_expr; const; var]
            in
            let lambda1 =
              lift2 Ctors.lambda
                (right_of (_lambda *> lcb) lambda_params)
                (left_of rcb prog) in
            let lambda2 =
              lift2 Ctors.lambda
                (token "->" *> parens params)
                (between lcb rcb prog) in
            let lambda_call lambda =
              lift2 Ctors.proc_call lambda (_dot *> token "call" *> parens args)
            in
            let power =
              let predict =
                ws *> peek_char_fail
                >>= function
                | '(' -> parens expr
                | '[' -> list
                | 'l' -> choice [lambda_call lambda1; lambda1 >>| Ctors.proc]
                | '-' ->
                    choice [lambda_call lambda2; lambda2 >>| Ctors.proc; inverse]
                | _ -> other in
              choice [predict; other] in
            let factor = chainr1 power _pow in
            let term = chainl1 factor factop in
            let arexpr = chainl1 term termop in
            let compare = chainl1 arexpr cmpop in
            let bfactor =
              fix (fun bfactor ->
                  let not = _not <* space <*> bfactor in
                  choice [not; compare]) in
            let bterm = chainl1 bfactor (_and <* space) in
            chainl1 bterm (_or <* space)) in
      let stmt =
        fix (fun stmt ->
            let empty : stmt list = [] in
            let list_item =
              let index = between lsb rsb expr in
              let rec take_index idx ctor =
                idx
                >>= fun a ->
                take_index idx (Ctors.list_item @@ ctor a) <|> return (ctor a)
              in
              var_name >>= fun name -> take_index index (Ctors.list_item name)
            in
            let lvalues = sep_by1 _comma (list_item <|> var_name) in
            let values = sep_by _comma expr in
            let values1 = sep_by1 _comma expr in
            let assign = lift2 Ctors.assign (lvalues <* token "=") values1 in
            let stmts = sep_by seps1 stmt in
            let if_expr = right_of _if (expr <* choice [_then; _eol; _semi]) in
            let while_expr =
              right_of _while (expr <* choice [_do; _eol; _semi]) in
            let until_expr =
              right_of _until (expr <* choice [_do; _eol; _semi]) in
            let if_stmt =
              lift3 Ctors.if_stmt if_expr stmts
                (left_of _end
                   (right_of (space *> _else) stmts <|> return empty)) in
            let expr_stmt = expr >>| Ctors.expr_stmt in
            let return_stmt = _return *> values >>| Ctors.return in
            let assoc_method_def =
              let assoc = list_item >>| Ctors.assoc_var <|> assoc_var <* _dot in
              lift4 Ctors.assoc_method_def (right_of _def assoc) method_id
                (parens params <* eol)
                (left_of _end stmts) in
            let method_def =
              lift3 Ctors.method_def (right_of _def method_id)
                (parens params <* eol)
                (left_of _end stmts) in
            let class_def =
              lift2 Ctors.class_def (right_of _class class_id)
                (left_of _end stmts) in
            let while_stmt =
              lift2 Ctors.while_stmt while_expr (left_of _end stmts) in
            let until_stmt =
              lift2 Ctors.until_stmt until_expr (left_of _end stmts) in
            let other = choice [assign; expr_stmt] in
            let predict =
              peek_char_fail
              >>= function
              | 'w' -> while_stmt
              | 'u' -> until_stmt
              | 'i' -> if_stmt
              | 'c' -> class_def
              | 'n' -> _next
              | 'b' -> _break
              | 'd' -> assoc_method_def <|> method_def
              | 'r' -> return_stmt
              | _ -> other in
            space *> choice [predict; other]) in
      sep_by seps1 stmt <* space)
