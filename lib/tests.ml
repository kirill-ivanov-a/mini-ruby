open Parser

let%test _ = parse id "some_id" = Ok "some_id"
let%test _ = parse id "while" = Error ": Invalid id"
let%test _ = parse number "42" = Ok (Number (Int 42))

let%test _ =
  parse number "42.0000000000000000000000000000000000" = Ok (Number (Float 42.))

let%test _ =
  parse prog "(a[f(1)] + 5 * f(2)) / 5 % 2 ** 5 ** 10"
  = Ok
      [ Expr
          (ArithOp
             ( Mod
             , ArithOp
                 ( Div
                 , ArithOp
                     ( Add
                     , Var
                         (ListItem
                            ( VarName (Local, "a")
                            , MethodCall ("f", [Number (Int 1)]) ))
                     , ArithOp
                         ( Mul
                         , Number (Int 5)
                         , MethodCall ("f", [Number (Int 2)]) ) )
                 , Number (Int 5) )
             , ArithOp
                 ( Pow
                 , Number (Int 2)
                 , ArithOp (Pow, Number (Int 5), Number (Int 10)) ) )) ]

let%test _ =
  parse prog "a[obj.f(1)]"
  = Ok
      [ Expr
          (Var
             (ListItem
                ( VarName (Local, "a")
                , AssocMethodCall
                    (AssocVar (VarName (Local, "obj")), "f", [Number (Int 1)])
                ))) ]

let%test _ =
  parse prog "1 * 2 / 3 % 4 * 5"
  = Ok
      [ Expr
          (ArithOp
             ( Mul
             , ArithOp
                 ( Mod
                 , ArithOp
                     ( Div
                     , ArithOp (Mul, Number (Int 1), Number (Int 2))
                     , Number (Int 3) )
                 , Number (Int 4) )
             , Number (Int 5) )) ]

let%test _ =
  parse prog "1 ** 2 ** 3 ** 4 ** 5"
  = Ok
      [ Expr
          (ArithOp
             ( Pow
             , Number (Int 1)
             , ArithOp
                 ( Pow
                 , Number (Int 2)
                 , ArithOp
                     ( Pow
                     , Number (Int 3)
                     , ArithOp (Pow, Number (Int 4), Number (Int 5)) ) ) )) ]

let%test _ =
  parse prog "f(f(f(f(f(f(f(f(f(f(f(f(1), f(2), f(3))))))))))))"
  = Ok
      [ Expr
          (MethodCall
             ( "f"
             , [ MethodCall
                   ( "f"
                   , [ MethodCall
                         ( "f"
                         , [ MethodCall
                               ( "f"
                               , [ MethodCall
                                     ( "f"
                                     , [ MethodCall
                                           ( "f"
                                           , [ MethodCall
                                                 ( "f"
                                                 , [ MethodCall
                                                       ( "f"
                                                       , [ MethodCall
                                                             ( "f"
                                                             , [ MethodCall
                                                                   ( "f"
                                                                   , [ MethodCall
                                                                         ( "f"
                                                                         , [ MethodCall
                                                                               ( 
                                                                               "f"
                                                                               , 
                                                                               [ 
                                                                               Number
                                                                                (
                                                                                Int
                                                                                1)
                                                                               ]
                                                                               )
                                                                           ; MethodCall
                                                                               ( 
                                                                               "f"
                                                                               , 
                                                                               [ 
                                                                               Number
                                                                                (
                                                                                Int
                                                                                2)
                                                                               ]
                                                                               )
                                                                           ; MethodCall
                                                                               ( 
                                                                               "f"
                                                                               , 
                                                                               [ 
                                                                               Number
                                                                                (
                                                                                Int
                                                                                3)
                                                                               ]
                                                                               )
                                                                           ] )
                                                                     ] ) ] ) ]
                                                       ) ] ) ] ) ] ) ] ) ] ) ]
                   ) ] )) ]

let%test _ =
  parse prog "[ f(), [1, 2, [1]], [], x + y ]"
  = Ok
      [ Expr
          (List
             [ MethodCall ("f", [])
             ; List [Number (Int 1); Number (Int 2); List [Number (Int 1)]]
             ; List []
             ; ArithOp
                 (Add, Var (VarName (Local, "x")), Var (VarName (Local, "y")))
             ]) ]

let%test _ =
  parse prog "@instance" = Ok [Expr (Var (VarName (Instance, "instance")))]

let%test _ =
  parse prog "[1, 2,\n3, 4, 5, \n6, 7, 8,\n9, 10, 11, 23\n]"
  = Ok
      [ Expr
          (List
             [ Number (Int 1); Number (Int 2); Number (Int 3); Number (Int 4)
             ; Number (Int 5); Number (Int 6); Number (Int 7); Number (Int 8)
             ; Number (Int 9); Number (Int 10); Number (Int 11); Number (Int 23)
             ]) ]

let%test _ =
  parse prog "x + \ny * \nz"
  = Ok
      [ Expr
          (ArithOp
             ( Add
             , Var (VarName (Local, "x"))
             , ArithOp
                 (Mul, Var (VarName (Local, "y")), Var (VarName (Local, "z")))
             )) ]

let%test _ =
  parse prog "\n->(x, y, z){\n    x + \n    y + \n    z\n}\n"
  = Ok
      [ Expr
          (Proc
             (Lambda
                ( ["x"; "y"; "z"]
                , [ Expr
                      (ArithOp
                         ( Add
                         , ArithOp
                             ( Add
                             , Var (VarName (Local, "x"))
                             , Var (VarName (Local, "y")) )
                         , Var (VarName (Local, "z")) )) ] ))) ]

let%test _ =
  parse prog "\n->(x, y, z){\n    x + y + z\n}\n"
  = Ok
      [ Expr
          (Proc
             (Lambda
                ( ["x"; "y"; "z"]
                , [ Expr
                      (ArithOp
                         ( Add
                         , ArithOp
                             ( Add
                             , Var (VarName (Local, "x"))
                             , Var (VarName (Local, "y")) )
                         , Var (VarName (Local, "z")) )) ] ))) ]

let%test _ =
  parse prog "lambda {\n    |x, y, z| \n    x + y + z \n    }"
  = Ok
      [ Expr
          (Proc
             (Lambda
                ( ["x"; "y"; "z"]
                , [ Expr
                      (ArithOp
                         ( Add
                         , ArithOp
                             ( Add
                             , Var (VarName (Local, "x"))
                             , Var (VarName (Local, "y")) )
                         , Var (VarName (Local, "z")) )) ] ))) ]

let%test _ =
  parse prog "lambda {\n    |x, y, z| \n    x + y + z \n    }.call(1, 2, 3)"
  = Ok
      [ Expr
          (ProcCall
             ( Lambda
                 ( ["x"; "y"; "z"]
                 , [ Expr
                       (ArithOp
                          ( Add
                          , ArithOp
                              ( Add
                              , Var (VarName (Local, "x"))
                              , Var (VarName (Local, "y")) )
                          , Var (VarName (Local, "z")) )) ] )
             , [Number (Int 1); Number (Int 2); Number (Int 3)] )) ]

let%test _ =
  parse prog "(x > y and x <= z) or true and not not not (x != -1)"
  = Ok
      [ Expr
          (BoolOp
             ( Or
             , BoolOp
                 ( And
                 , CmpOp
                     (Gt, Var (VarName (Local, "x")), Var (VarName (Local, "y")))
                 , CmpOp
                     ( LtE
                     , Var (VarName (Local, "x"))
                     , Var (VarName (Local, "z")) ) )
             , BoolOp
                 ( And
                 , True
                 , UnaryOp
                     ( Not
                     , UnaryOp
                         ( Not
                         , UnaryOp
                             ( Not
                             , CmpOp
                                 ( NotEq
                                 , Var (VarName (Local, "x"))
                                 , UnaryOp (Inv, Number (Int 1)) ) ) ) ) ) )) ]

let%test _ =
  parse prog "self.do_something(self.x(), self.y())"
  = Ok
      [ Expr
          (AssocMethodCall
             ( Self
             , "do_something"
             , [AssocMethodCall (Self, "x", []); AssocMethodCall (Self, "y", [])]
             )) ]

let%test _ = parse prog "\"string\"" = Ok [Expr (String [Literal "string"])]

let%test _ =
  parse prog "\"#{\n f() + g ()\n  }str#{ i }ng\""
  = Ok
      [ Expr
          (String
             [ Substitution
                 (ArithOp (Add, MethodCall ("f", []), MethodCall ("g", [])))
             ; Literal "str"; Substitution (Var (VarName (Local, "i")))
             ; Literal "ng" ]) ]

let%test _ =
  parse prog
    "\n\
     while x\n\
    \ until y do\n\
    \   while x do\n\
    \    x, y, z = [x, y, z]\n\
    \   end\n\
    \   f(x)\n\
    \ end\n\
    \ f(y)\n\
     end\n"
  = Ok
      [ While
          ( Var (VarName (Local, "x"))
          , [ Until
                ( Var (VarName (Local, "y"))
                , [ While
                      ( Var (VarName (Local, "x"))
                      , [ Assign
                            ( [ VarName (Local, "x"); VarName (Local, "y")
                              ; VarName (Local, "z") ]
                            , [ List
                                  [ Var (VarName (Local, "x"))
                                  ; Var (VarName (Local, "y"))
                                  ; Var (VarName (Local, "z")) ] ] ) ] )
                  ; Expr (MethodCall ("f", [Var (VarName (Local, "x"))])) ] )
            ; Expr (MethodCall ("f", [Var (VarName (Local, "y"))])) ] ) ]

let%test _ =
  parse prog
    {|
class Point
  def initialize(x, y)
   @x = x
   @y = y
  end

  def x()
   @x
  end
  
  def y()
   return @y
  end

  def x=(val)
   @x = val
  end

  def y=(val)
   @y = val
  end
end
point = Point.new(1, 2)
point.x=(5)
point.x()
|}
  = Ok
      [ ClassDef
          ( "Point"
          , [ MethodDef
                ( "initialize"
                , ["x"; "y"]
                , [ Assign
                      ([VarName (Instance, "x")], [Var (VarName (Local, "x"))])
                  ; Assign
                      ([VarName (Instance, "y")], [Var (VarName (Local, "y"))])
                  ] )
            ; MethodDef ("x", [], [Expr (Var (VarName (Instance, "x")))])
            ; MethodDef ("y", [], [Return [Var (VarName (Instance, "y"))]])
            ; MethodDef
                ( "x="
                , ["val"]
                , [ Assign
                      ([VarName (Instance, "x")], [Var (VarName (Local, "val"))])
                  ] )
            ; MethodDef
                ( "y="
                , ["val"]
                , [ Assign
                      ([VarName (Instance, "y")], [Var (VarName (Local, "val"))])
                  ] ) ] )
      ; Assign
          ( [VarName (Local, "point")]
          , [ AssocMethodCall
                (ClassName "Point", "new", [Number (Int 1); Number (Int 2)]) ]
          )
      ; Expr
          (AssocMethodCall
             (AssocVar (VarName (Local, "point")), "x=", [Number (Int 5)]))
      ; Expr (AssocMethodCall (AssocVar (VarName (Local, "point")), "x", [])) ]

let%test _ =
  parse prog
    {|




          def obj.sigleton_method(x, y, z)
            return x + y + z
          end
          def Point.x_sum(p1, p2, p3)
            return p1.x() + p2.x() + p3.x()
          end






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|}
  = Ok
      [ AssocMethodDef
          ( AssocVar (VarName (Local, "obj"))
          , "sigleton_method"
          , ["x"; "y"; "z"]
          , [ Return
                [ ArithOp
                    ( Add
                    , ArithOp
                        ( Add
                        , Var (VarName (Local, "x"))
                        , Var (VarName (Local, "y")) )
                    , Var (VarName (Local, "z")) ) ] ] )
      ; AssocMethodDef
          ( ClassName "Point"
          , "x_sum"
          , ["p1"; "p2"; "p3"]
          , [ Return
                [ ArithOp
                    ( Add
                    , ArithOp
                        ( Add
                        , AssocMethodCall
                            (AssocVar (VarName (Local, "p1")), "x", [])
                        , AssocMethodCall
                            (AssocVar (VarName (Local, "p2")), "x", []) )
                    , AssocMethodCall (AssocVar (VarName (Local, "p3")), "x", [])
                    ) ] ] ) ]

let%test _ =
  parse prog
    {|
def fact(n)
    if n == 1
      return 1
    else
      return n * fact (n - 1) 
    end
end
|}
  = Ok
      [ MethodDef
          ( "fact"
          , ["n"]
          , [ If
                ( CmpOp (Eq, Var (VarName (Local, "n")), Number (Int 1))
                , [Return [Number (Int 1)]]
                , [ Return
                      [ ArithOp
                          ( Mul
                          , Var (VarName (Local, "n"))
                          , MethodCall
                              ( "fact"
                              , [ ArithOp
                                    ( Sub
                                    , Var (VarName (Local, "n"))
                                    , Number (Int 1) ) ] ) ) ] ] ) ] ) ]
