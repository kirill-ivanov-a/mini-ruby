open Ast
open Format

type value =
  | Vnil
  | Vbool of bool
  | Vint of int
  | Vfloat of float
  | Vstring of string
  | Vobject of int
  | Vlist of list_itms
  | VlistRef of int

and list_itms = value list

module Ctors = struct
  let vbool b = Vbool b
  let vint n = Vint n
  let vfloat f = Vfloat f
  let vstring s = Vstring s
  let vobj id = Vobject id
  let vlist lst = Vlist lst
  let vlistref id = VlistRef id
end

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

let create_id =
  let n = ref 0 in
  fun () ->
    let id = !n in
    if id = -1 then failwith "Counter reached max value.";
    incr n;
    id

type vars = (identifier, value) Hashtbl.t
type methods = (identifier, params * stmt list) Hashtbl.t
type scope_object = Instance of int | Class of identifier
type jump_stmt = None | Break | Next | Return of value

type class_ctx =
  {class_methods: methods; inst_methods: methods; class_vars: vars}

type inst_ctx = {class_name: identifier; inst_methods: methods; inst_vars: vars}

type exec_ctx =
  { scope_obj: scope_object
  ; local_vars: vars
  ; jump_stmt: jump_stmt
  ; last_value: value
  ; classes: (identifier, class_ctx) Hashtbl.t
  ; instances: (int, inst_ctx) Hashtbl.t
  ; lists: (int, list_itms) Hashtbl.t }

let rec string_of_value ctx =
  let pp_list lst =
    let pp_comma fmt () = pp_print_string fmt ", " in
    let string_of_list_itm = function
      | Vnil -> "nil"
      | Vstring s -> asprintf {|"%s"|} s
      | v -> string_of_value ctx v in
    let print_list_itm fmt v = fprintf fmt "%s" (string_of_list_itm v) in
    let pp_list fmt = pp_print_list ~pp_sep:pp_comma print_list_itm fmt in
    asprintf "[%a]" pp_list lst in
  function
  | Vnil -> ""
  | Vbool b -> string_of_bool b
  | Vint n -> string_of_int n
  | Vfloat n -> string_of_float n
  | Vstring s -> s
  | Vlist lst -> pp_list lst
  | Vobject id -> sprintf "object: %d" id
  | VlistRef id -> (
    match Hashtbl.find_opt ctx.lists id with Some lst -> pp_list lst | _ -> "" )

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let error = Result.error
end

module Eval (M : MONADERROR) = struct
  open M
  open Ctors

  let try_create_id () =
    try return (() |> create_id) with Failure m -> error m

  (* List functions *)
  let rec map f = function
    | [] -> return []
    | h :: tl -> f h >>= fun c -> map f tl >>= fun lst -> return @@ (c :: lst)

  let rec iter2 f l1 l2 =
    match (l1, l2) with
    | [], [] -> return ()
    | h1 :: tl1, h2 :: tl2 -> f h1 h2 >>= fun () -> iter2 f tl1 tl2
    | _, _ -> error "lists have different lengths"

  let rec fold_left f acc = function
    | [] -> return acc
    | h :: tl -> f acc h >>= fun acc -> fold_left f acc tl

  let lst_try_replace l pos a =
    let replace l pos a = List.mapi (fun i x -> if i = pos then a else x) l in
    match List.nth_opt l pos with
    | Some _ -> return @@ replace l pos a
    | _ -> error "index was outside the bounds of the list"

  (* Hashtable functions *)

  let ht_find_opt_and_process ht key f emsg =
    match Hashtbl.find_opt ht key with Some v -> f v | None -> error emsg

  let ht_try_replace ht k v =
    try return @@ Hashtbl.replace ht k v
    with Invalid_argument _ -> error "invalid argument"

  let find_class_method class_name method_name ctx =
    match Hashtbl.find_opt ctx.classes class_name with
    | Some class_ctx -> (
      match Hashtbl.find_opt class_ctx.class_methods method_name with
      | Some mthd -> return mthd
      | None -> error "method not found" )
    | None -> error "class not found"

  let find_inst_method inst_id method_name ctx =
    match Hashtbl.find_opt ctx.instances inst_id with
    | Some inst_ctx -> (
      (* поиск метода в контексте экзмепляра *)
      match Hashtbl.find_opt inst_ctx.inst_methods method_name with
      | Some mthd -> return mthd
      | None -> (
        (* если не найден, ищем в контексте класса *)
        match Hashtbl.find_opt ctx.classes inst_ctx.class_name with
        | None -> error "class not found"
        | Some class_ctx -> (
          match Hashtbl.find_opt class_ctx.inst_methods method_name with
          | Some mthd -> return mthd
          | None -> error "method not found" ) ) )
    | None -> error "instance not found"

  let add_ctor (params, body) class_name ctx =
    ht_find_opt_and_process ctx.classes class_name
      (fun class_ctx ->
        ht_try_replace class_ctx.class_methods "new" (params, body))
      "class not found"

  let is_false = function Vnil -> true | Vbool b -> not b | _ -> false
  let is_true v = not @@ is_false v

  let rec find_var ctx = function
    | VarName (Local, name) ->
        ht_find_opt_and_process ctx.local_vars name return
          "local variable not found"
    | VarName (Instance, name) -> (
      match ctx.scope_obj with
      | Class class_name ->
          ht_find_opt_and_process ctx.classes class_name
            (fun clss ->
              ht_find_opt_and_process clss.class_vars name return
                "variable not found")
            "class not found"
      | Instance inst_id ->
          ht_find_opt_and_process ctx.instances inst_id
            (fun inst ->
              ht_find_opt_and_process inst.inst_vars name return
                "variable not found")
            "instance not found" )
    | ListItem (var, idx) -> (
        find_var ctx var
        >>= function
        | VlistRef id -> (
            eval_expr ctx idx
            >>= function
            | Vint idx ->
                ht_find_opt_and_process ctx.lists id
                  (fun lst ->
                    match List.nth_opt lst idx with
                    | Some v -> return v
                    | None -> return Vnil)
                  "list not found"
            | _ -> error "index must be integer" )
        | _ -> error "indexer not defined for variable" )

  and eval_expr ctx = function
    | Number num -> (
      match num with Int i -> return @@ vint i | Float f -> return @@ vfloat f )
    | Var v -> find_var ctx v
    | String lst ->
        fold_left
          (fun s0 -> function Literal l -> return @@ s0 ^ l
            | Substitution e ->
                eval_expr ctx e
                >>= fun v -> return @@ s0 ^ string_of_value ctx v)
          "" lst
        >>= fun str -> return @@ vstring str
    | ProcCall (Lambda (params, block), args) ->
        try_set_local_vars params args ctx
        >>= fun vars ->
        eval_method {ctx with local_vars= vars; last_value= Vnil} block
    | Proc (Lambda (params, block)) ->
        let proc_method = (Hashtbl.create 16 : methods) in
        let vars = Hashtbl.create 16 in
        let help = Hashtbl.add proc_method "call" (params, block) in
        () |> try_create_id
        >>= fun inst_id ->
        Hashtbl.add ctx.instances inst_id
          {class_name= "Proc"; inst_methods= proc_method; inst_vars= vars};
        return @@ vobj @@ inst_id
    | ArithOp (op, e1, e2) ->
        let arithop =
          let find_list id =
            ht_find_opt_and_process ctx.lists id return "list not found" in
          let _add v1 v2 =
            let add_lists l1 l2 = return @@ vlist @@ List.concat [l1; l2] in
            match (v1, v2) with
            | Vint i1, Vint i2 -> return @@ vint @@ (i1 + i2)
            | Vstring s1, Vstring s2 -> return @@ vstring @@ s1 ^ s2
            | Vlist l1, Vlist l2 -> add_lists l1 l2
            | VlistRef id, Vlist l2 ->
                find_list id >>= fun l1 -> add_lists l1 l2
            | Vlist l1, VlistRef id ->
                find_list id >>= fun l2 -> add_lists l1 l2
            | VlistRef id1, VlistRef id2 ->
                find_list id1
                >>= fun l1 -> find_list id2 >>= fun l2 -> add_lists l1 l2
            | Vfloat f1, Vint i2 -> return @@ vfloat @@ (f1 +. Float.of_int i2)
            | Vint i1, Vfloat f2 -> return @@ vfloat @@ (Float.of_int i1 +. f2)
            | Vfloat f1, Vfloat f2 -> return @@ vfloat @@ (f1 +. f2)
            | _ -> error "unexpected operands for '+' operator" in
          let _mul v1 v2 =
            match (v1, v2) with
            | Vint i1, Vint i2 -> return @@ vint @@ (i1 * i2)
            | Vfloat f1, Vint i2 -> return @@ vfloat @@ (f1 *. Float.of_int i2)
            | Vint i1, Vfloat f2 -> return @@ vfloat @@ (Float.of_int i1 *. f2)
            | Vfloat f1, Vfloat f2 -> return @@ vfloat @@ (f1 *. f2)
            | _ -> error "unexpected operands for '*' operator" in
          let _sub v1 v2 =
            let sub_lists l1 l2 =
              return @@ vlist
              @@ List.filter (fun item -> not (List.mem item l2)) l1 in
            match (v1, v2) with
            | Vint i1, Vint i2 -> return @@ vint @@ (i1 - i2)
            | Vlist l1, Vlist l2 -> sub_lists l1 l2
            | VlistRef id, Vlist l2 ->
                find_list id >>= fun l1 -> sub_lists l1 l2
            | Vlist l1, VlistRef id ->
                find_list id >>= fun l2 -> sub_lists l1 l2
            | VlistRef id1, VlistRef id2 ->
                find_list id1
                >>= fun l1 -> find_list id2 >>= fun l2 -> sub_lists l1 l2
            | Vfloat f1, Vint i2 -> return @@ vfloat @@ (f1 -. Float.of_int i2)
            | Vint i1, Vfloat f2 -> return @@ vfloat @@ (Float.of_int i1 -. f2)
            | Vfloat f1, Vfloat f2 -> return @@ vfloat @@ (f1 -. f2)
            | _ -> error "unexpected operands for '-' operator" in
          let _div_or_mod op v1 v2 =
            let get_ops =
              match op with
              | Div -> return @@ (( / ), ( /. ))
              | Mod -> return @@ (( mod ), mod_float)
              | _ -> error "unexpected operator" in
            get_ops
            >>= fun ops ->
            let i_op = fst ops in
            let f_op = snd ops in
            match (v1, v2) with
            | Vfloat _, Vint 0
             |Vfloat _, Vfloat 0.0
             |Vint _, Vfloat 0.0
             |Vint _, Vint 0 ->
                error "division by zero"
            | Vint i1, Vint i2 -> return @@ vint @@ i_op i1 i2
            | Vfloat f1, Vint i2 ->
                return @@ vfloat @@ f_op f1 (Float.of_int i2)
            | Vint i1, Vfloat f2 ->
                return @@ vfloat @@ f_op (Float.of_int i1) f2
            | Vfloat f1, Vfloat f2 -> return @@ vfloat @@ f_op f1 f2
            | _ -> error "unexpected operands for '/' or '%' operator" in
          let _pow v1 v2 =
            match (v1, v2) with
            | Vint i1, Vint i2 ->
                return @@ vfloat @@ (Float.of_int i1 ** Float.of_int i2)
            | Vfloat f1, Vint i2 -> return @@ vfloat @@ (f1 ** Float.of_int i2)
            | Vint i1, Vfloat f2 -> return @@ vfloat @@ (Float.of_int i1 ** f2)
            | Vfloat f1, Vfloat f2 -> return @@ vfloat @@ (f1 ** f2)
            | _ -> error "unexpected operands for '**' operator" in
          function
          | Add -> _add
          | Sub -> _sub
          | Div -> _div_or_mod Div
          | Mod -> _div_or_mod Mod
          | Mul -> _mul
          | Pow -> _pow in
        eval_expr ctx e1
        >>= fun v1 -> eval_expr ctx e2 >>= fun v2 -> arithop op v1 v2
    | UnaryOp (Inv, e) -> (
        eval_expr ctx e
        >>= function
        | Vint i -> return @@ vint (-i)
        | Vfloat f -> return @@ vfloat (-.f)
        | _ -> error "invalid type for '-' operator" )
    | UnaryOp (Not, e) ->
        eval_expr ctx e >>= fun v -> return @@ vbool @@ is_false v
    | True -> return @@ vbool true
    | False -> return @@ vbool false
    | Nil -> return Vnil
    | BoolOp (op, e1, e2) ->
        let boolop = function And -> ( && ) | Or -> ( || ) in
        eval_expr ctx e1
        >>= fun v1 ->
        eval_expr ctx e2
        >>= fun v2 -> return @@ vbool (boolop op (is_true v1) (is_true v2))
    | CmpOp (op, e1, e2) ->
        let rec compare_value v1 v2 =
          let compare_lists l1 l2 = function
            | Eq | NotEq ->
                let cmp_len = List.compare_lengths l1 l2 in
                let rec compare_vlists = function
                  | [], [] -> return 0
                  | h1 :: tl1, h2 :: tl2 -> (
                      compare_value h1 h2
                      >>= function
                      | 0 -> compare_vlists (tl1, tl2) | res -> return res )
                  | _, _ -> error "lists have different lengths" in
                if cmp_len != 0 then return @@ cmp_len
                else compare_vlists (l1, l2)
            | _ -> error "unexpected comparison operator for lists" in
          match (v1, v2) with
          | Vnil, Vnil -> (
            match op with
            | Eq -> return 0
            | NotEq -> return 1
            | _ -> error "unexpected comparison operator for nils" )
          | Vbool b1, Vbool b2 -> (
            match op with
            | Eq | NotEq -> return @@ compare b1 b2
            | _ -> error "unexpected comparison operator for bool values" )
          | Vint i1, Vint i2 -> return @@ compare i1 i2
          | Vfloat f1, Vfloat f2 -> return @@ compare f1 f2
          | Vfloat f, Vint i -> return @@ compare f (Float.of_int i)
          | Vint i, Vfloat f -> return @@ compare (Float.of_int i) f
          | Vstring s1, Vstring s2 -> return @@ compare s1 s2
          | Vlist l1, Vlist l2 -> compare_lists l1 l2 op
          | VlistRef lref, Vlist l1 | Vlist l1, VlistRef lref ->
              ht_find_opt_and_process ctx.lists lref
                (fun l2 -> compare_lists l1 l2 op)
                "list not found"
          | VlistRef lr1, VlistRef lr2 ->
              ht_find_opt_and_process ctx.lists lr1
                (fun l1 ->
                  ht_find_opt_and_process ctx.lists lr2
                    (fun l2 -> compare_lists l1 l2 op)
                    "list not found")
                "list not found"
          | Vobject id1, Vobject id2 -> return @@ compare id1 id2
          | _ -> error "unexpected types for compare" in
        let cmpop = function
          | Eq -> ( == )
          | NotEq -> ( != )
          | Lt -> ( < )
          | LtE -> ( <= )
          | Gt -> ( > )
          | GtE -> ( >= ) in
        eval_expr ctx e1
        >>= fun v1 ->
        eval_expr ctx e2
        >>= fun v2 ->
        compare_value v1 v2 >>= fun res -> return @@ vbool @@ (cmpop op) res 0
    | MethodCall (method_name, args) -> (
        let try_eval_method (params, body) =
          try_set_local_vars params args ctx
          >>= fun vars ->
          eval_method {ctx with local_vars= vars; last_value= Vnil} body in
        match ctx.scope_obj with
        | Class class_name ->
            find_class_method class_name method_name ctx
            >>= fun m -> try_eval_method m
        | Instance inst_id ->
            find_inst_method inst_id method_name ctx
            >>= fun m -> try_eval_method m )
    | AssocMethodCall (assoc_var, method_name, args) -> (
        let try_eval_method inst_id =
          find_inst_method inst_id method_name ctx
          >>= fun (params, body) ->
          try_set_local_vars params args ctx
          >>= fun vars ->
          eval_method
            { ctx with
              scope_obj= Instance inst_id
            ; local_vars= vars
            ; last_value= Vnil }
            body in
        match assoc_var with
        | AssocVar v -> (
            find_var ctx v
            >>= function
            | Vobject inst_id -> try_eval_method inst_id
            | _ -> error "failed to call method" )
        | ClassName class_name ->
            find_class_method class_name method_name ctx
            >>= fun (params, body) ->
            if method_name = "new" then
              init_inst ctx class_name args (params, body)
            else
              try_set_local_vars params args ctx
              >>= fun vars ->
              eval_method
                { ctx with
                  scope_obj= Class class_name
                ; local_vars= vars
                ; jump_stmt= None
                ; last_value= Vnil }
                body
        | _ -> error "invalid variable" )
    | List itms ->
        map (fun e -> eval_expr ctx e) itms >>= fun lst -> return @@ vlist lst

  and init_inst ctx c_name args (params, body) =
    ht_find_opt_and_process ctx.classes c_name return "class not found"
    >>= fun c_ctx ->
    return @@ Hashtbl.copy c_ctx.inst_methods
    >>= fun i_methods ->
    () |> try_create_id
    >>= fun inst_id ->
    try_set_local_vars params args ctx
    >>= fun vars ->
    let init_ctx =
      { ctx with
        scope_obj= Instance inst_id
      ; local_vars= vars
      ; jump_stmt= None
      ; last_value= Vnil } in
    let add_inst =
      Hashtbl.add ctx.instances inst_id
        { class_name= c_name
        ; inst_methods= i_methods
        ; inst_vars= Hashtbl.create 16 } in
    let vobj = Vobject inst_id in
    eval_method init_ctx body >>= fun _ -> return vobj

  and assign targets values ctx =
    let assign t v =
      let rec make_refs lst = map check_lists lst
      and check_lists = function
        | Vlist lst ->
            make_refs lst
            >>= fun new_lst ->
            () |> try_create_id
            >>= fun idx ->
            ht_try_replace ctx.lists idx new_lst
            >>= fun _ -> return @@ vlistref @@ idx
        | _ as v -> return v in
      match t with
      | VarName (Local, name) ->
          check_lists v
          >>= fun v ->
          ht_try_replace ctx.local_vars name v >>= fun _ -> return ctx
      | ListItem (var, idx) -> (
          find_var ctx var
          >>= function
          | VlistRef id -> (
              eval_expr ctx idx
              >>= function
              | Vint idx ->
                  ht_find_opt_and_process ctx.lists id
                    (fun lst ->
                      check_lists v
                      >>= fun v ->
                      lst_try_replace lst idx v
                      >>= fun new_lst -> ht_try_replace ctx.lists id new_lst)
                    "list not found"
                  >>= fun _ -> return ctx
              | _ -> error "index must be integer" )
          | _ -> error "indexer not defined for variable" )
      | VarName (Instance, name) -> (
        match ctx.scope_obj with
        | Class class_name -> (
          match Hashtbl.find_opt ctx.classes class_name with
          | Some class_ctx ->
              check_lists v
              >>= fun v ->
              ht_try_replace class_ctx.class_vars name v >>= fun _ -> return ctx
          | None -> error "class not found" )
        | Instance id -> (
          match Hashtbl.find_opt ctx.instances id with
          | Some inst_ctx ->
              check_lists v
              >>= fun v ->
              ht_try_replace inst_ctx.inst_vars name v >>= fun _ -> return ctx
          | None -> error "instance not found" ) ) in
    let rec helper targets values =
      match targets with
      | [] -> return ctx
      | [t] -> (
        match values with
        | [v] -> assign t v >>= fun _ -> return ctx
        | v :: _ -> assign t v >>= fun _ -> return ctx
        | [] -> assign t Vnil >>= fun _ -> return ctx )
      | t :: ts -> (
        match values with
        | [e] -> (
          match e with
          | Vlist (v :: vs) -> assign t v >>= fun _ -> helper ts vs
          | _ as v -> assign t v >>= fun _ -> helper ts [] )
        | v :: vs -> assign t v >>= fun _ -> helper ts vs
        | [] -> assign t Vnil >>= fun _ -> helper ts [] ) in
    map (fun e -> eval_expr ctx e) values >>= fun vals -> helper targets vals

  and try_set_local_vars params args ctx =
    let vars = Hashtbl.create 16 in
    if List.compare_lengths args params != 0 then error "invalid args number"
    else
      map (fun e -> eval_expr ctx e) args
      >>= fun vals ->
      iter2 (fun id v -> return @@ Hashtbl.replace vars id v) params vals
      >>= fun _ -> return vars

  and eval_stmt ctx = function
    | Expr e -> eval_expr ctx e >>= fun v -> return {ctx with last_value= v}
    | MethodDef (method_name, params, body) -> (
        let inst_method_def class_name =
          ht_find_opt_and_process ctx.classes class_name
            (fun class_ctx ->
              ht_try_replace class_ctx.inst_methods method_name (params, body)
              >>= fun _ -> return ctx)
            "class not found" in
        match ctx.scope_obj with
        | Class class_name ->
            if method_name = "initialize" then
              add_ctor (params, body) class_name ctx >>= fun _ -> return ctx
            else inst_method_def class_name
        | Instance inst_id ->
            ht_find_opt_and_process ctx.instances inst_id
              (fun inst_ctx ->
                let class_name = inst_ctx.class_name in
                if method_name = "initialize" then
                  add_ctor (params, body) class_name ctx >>= fun _ -> return ctx
                else inst_method_def class_name)
              "instance not found" )
    | AssocMethodDef (assoc_var, method_name, params, body) -> (
        let class_method_def class_name =
          ht_find_opt_and_process ctx.classes class_name
            (fun class_ctx ->
              ht_try_replace class_ctx.class_methods method_name (params, body)
              >>= fun _ -> return ctx)
            "class not found" in
        match assoc_var with
        | ClassName name -> class_method_def name
        | Self -> (
          match ctx.scope_obj with
          | Class name -> class_method_def name
          | _ -> error "unexpected scope object" )
        | AssocVar v -> (
            find_var ctx v
            >>= function
            | Vobject inst_id ->
                ht_find_opt_and_process ctx.instances inst_id
                  (fun inst_ctx ->
                    ht_try_replace inst_ctx.inst_methods method_name
                      (params, body)
                    >>= fun _ -> return ctx)
                  "instance not found"
            | _ -> error "method definition error" ) )
    | ClassDef (class_name, block) ->
        let c_methods = Hashtbl.create 16 in
        let i_methods = Hashtbl.create 16 in
        let c_vars = Hashtbl.create 16 in
        ht_try_replace c_methods "new" ([], [])
        >>= fun _ ->
        ht_try_replace ctx.classes class_name
          {class_methods= c_methods; inst_methods= i_methods; class_vars= c_vars}
        >>= fun _ ->
        let exec_ctx =
          { ctx with
            scope_obj= Class class_name
          ; local_vars= Hashtbl.create 16
          ; jump_stmt= None
          ; last_value= Vnil } in
        eval_block exec_ctx block
    | If (e, if_block, else_block) ->
        eval_expr ctx e
        >>= fun c ->
        if is_true c then eval_if ctx if_block else eval_if ctx else_block
    | Assign (targets, values) -> assign targets values ctx
    | While (expr, stmts) -> eval_cycle ctx expr stmts is_true
    | Until (expr, stmts) -> eval_cycle ctx expr stmts is_false
    | Break -> return {ctx with jump_stmt= Break}
    | Next -> return {ctx with jump_stmt= Next}
    | Return exprs -> eval_return exprs ctx

  and check_stmt ctx emsg = function
    | ClassDef _ | MethodDef _ | AssocMethodDef _ -> error emsg
    | _ as stmt -> eval_stmt ctx stmt

  and eval_return exprs ctx =
    match exprs with
    | [] -> return {ctx with jump_stmt= Return Vnil; last_value= Vnil}
    | [expr] ->
        eval_expr ctx expr
        >>= fun v -> return {ctx with jump_stmt= Return v; last_value= v}
    | _ as expr_list ->
        map (fun expr -> eval_expr ctx expr) expr_list
        >>= fun lst ->
        let ret_lst = Vlist lst in
        return {ctx with jump_stmt= Return ret_lst; last_value= ret_lst}

  and eval_method ctx = function
    | [] -> return ctx.last_value
    | [Ast.Return exprs] ->
        eval_return exprs ctx >>= fun c -> return c.last_value
    | stmt :: stmts -> (
        check_stmt ctx "unexpected statement in method body" stmt
        >>= fun c ->
        let bs = c.jump_stmt in
        match bs with
        | Return v -> return v
        | None -> eval_method c stmts
        | _ -> error "unexpected break statement" )

  and eval_if ctx = function
    | [] -> return {ctx with last_value= Vnil}
    | stmt :: stmts -> (
        check_stmt ctx "unexpected statement in if-else body" stmt
        >>= fun c ->
        match c.jump_stmt with
        | None -> eval_block c stmts
        | Return _ | Break | Next -> return c )

  and eval_block ctx = function
    | [] -> return ctx
    | [stmt] -> eval_stmt ctx stmt >>= fun c -> return c
    | h :: tl -> (
        eval_stmt ctx h
        >>= fun c ->
        match c.jump_stmt with
        | None -> eval_block c tl
        | Return _ | Break | Next -> return c )

  and eval_cycle ctx cond_expr stmts check_cond =
    let rec helper ctx stmts =
      eval_expr ctx cond_expr
      >>= fun v ->
      if check_cond v then eval_stmts ctx stmts
      else return {ctx with last_value= Vnil}
    and eval_stmts _ctx = function
      | [] -> helper _ctx stmts
      | stmt :: stmts -> (
          check_stmt _ctx "unexpected statement in cycle body" stmt
          >>= fun c ->
          match c.jump_stmt with
          | Break -> return {c with jump_stmt= None; last_value= Vnil}
          | Next -> helper c stmts
          | Return _ -> return c
          | None -> eval_stmts c stmts ) in
    helper ctx stmts

  and eval_prog ctx = function
    | [] -> return Vnil
    | [x] -> (
        eval_stmt ctx x
        >>= fun c ->
        match c.jump_stmt with
        | None -> return c.last_value
        | _ -> error "unexpected jump statement" )
    | stmt :: stmts -> (
        eval_stmt ctx stmt
        >>= fun c ->
        match c.jump_stmt with
        | None -> eval_prog c stmts
        | _ -> error "unexpected jump statement" )
end

let make_exec_ctx () =
  let _classes = Hashtbl.create 16 in
  let _instances = Hashtbl.create 16 in
  let init_classes =
    Hashtbl.add _classes "Object"
      { class_methods= Hashtbl.create 16
      ; inst_methods= Hashtbl.create 16
      ; class_vars= Hashtbl.create 16 } in
  let main_id = () |> create_id in
  let init_main =
    Hashtbl.add _instances main_id
      { class_name= "Object"
      ; inst_methods= Hashtbl.create 16
      ; inst_vars= Hashtbl.create 16 } in
  { scope_obj= Instance main_id
  ; local_vars= Hashtbl.create 16
  ; jump_stmt= None
  ; last_value= Vnil
  ; classes= _classes
  ; instances= _instances
  ; lists= Hashtbl.create 16 }

let eval ctx =
  let module E = Eval (Result) in
  E.eval_prog ctx

let print_value ctx = function
  | Vnil -> print_endline "nil"
  | Vstring s -> print_endline @@ asprintf {|"%s"|} s
  | v -> print_endline @@ string_of_value ctx v

let copy_ctx ctx =
  { ctx with
    local_vars= Hashtbl.copy ctx.local_vars
  ; classes= Hashtbl.copy ctx.classes
  ; instances= Hashtbl.copy ctx.instances
  ; lists= Hashtbl.copy ctx.lists }
