Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

  $ (cd ../../../../default && demos/demoParser.exe)
  ClassDef
  ("Point",
   [MethodDef
    ("initialize", ["x"; "y"],
     [Assign ([VarName (Instance, "x")], [Var (VarName (Local, "x"))]);
      Assign ([VarName (Instance, "y")], [Var (VarName (Local, "y"))])]);
    MethodDef ("x", [], [Expr (Var (VarName (Instance, "x")))]);
    MethodDef ("y", [], [Expr (Var (VarName (Instance, "y")))]);
    MethodDef
    ("x=", ["v"],
     [Assign ([VarName (Instance, "x")], [Var (VarName (Local, "v"))])]);
    MethodDef
    ("y=", ["v"],
     [Assign ([VarName (Instance, "y")], [Var (VarName (Local, "v"))])]);
    AssocMethodDef
    (ClassName ("Point"), "distance", ["p1"; "p2"],
     [Return ([MethodCall
               ("sqrt",
                [ArithOp
                 (Add,
                  ArithOp
                  (Pow,
                   ArithOp
                   (Sub,
                    AssocMethodCall (AssocVar (VarName (Local, "p1")), "x", []),
                    AssocMethodCall (AssocVar (VarName (Local, "p2")), "x", [])),
                   Number (Int (2))),
                  ArithOp
                  (Pow,
                   ArithOp
                   (Sub,
                    AssocMethodCall (AssocVar (VarName (Local, "p1")), "y", []),
                    AssocMethodCall (AssocVar (VarName (Local, "p2")), "y", [])),
                   Number (Int (2))))])])]);
    MethodDef
    ("distance_to_origin", [],
     [Return ([MethodCall
               ("sqrt",
                [ArithOp
                 (Add,
                  ArithOp
                  (Pow, Var (VarName (Instance, "x")), Number (Int (2))),
                  ArithOp
                  (Pow, Var (VarName (Instance, "y")), Number (Int (2))))])])]);
    MethodDef
    ("to_s", [],
     [Return ([String ([Literal ("(");
                        Substitution (Var (VarName (Instance, "x")));
                        Literal (", ");
                        Substitution (Var (VarName (Instance, "y")));
                        Literal (")")])])])])
  Assign
  ([VarName (Local, "origin")],
   [AssocMethodCall
    (ClassName ("Point"), "new", [Number (Int (0)); Number (Int (0))])])
  Expr (MethodCall ("puts", [Var (VarName (Local, "origin"))]))
  Assign
  ([VarName (Local, "p1")],
   [AssocMethodCall
    (ClassName ("Point"), "new", [Number (Float (20.)); Number (Float (5.))])])
  Assign
  ([VarName (Local, "p2")],
   [AssocMethodCall
    (ClassName ("Point"), "new", [Number (Float (5.)); Number (Float (4.))])])
  Assign
  ([VarName (Local, "d")],
   [AssocMethodCall
    (ClassName ("Point"), "distance",
     [Var (VarName (Local, "p1")); Var (VarName (Local, "p2"))])])
  $ (cd ../../../../default && demos/demoRecursionFact.exe)
  [1, 1, 2, 6, 24, 120]

  $ (cd ../../../../default && demos/demoRecursionFib.exe)
  [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

  $ (cd ../../../../default && demos/demoArithmeticNumbers.exe)
  [65536., 15.5, -5.5, 3, 3.33333333333, 30, 30., 1, 0.]

  $ (cd ../../../../default && demos/demoArithStringList.exe)
  [[1, 2, 3], [4, 5], [1, 2, 3, 4, 5], [1, 2, 3], [1, 2, 3, 4, 5], "str", "ing", "string"]

  $ (cd ../../../../default && demos/demoBoolCmpOp.exe)
  [true, true, false, false, false, true, false, true, false, true, false, false]

  $ (cd ../../../../default && demos/demoLambda.exe)
  17

  $ (cd ../../../../default && demos/demoCycle.exe)
  10

  $ (cd ../../../../default && demos/demoObjects.exe)
  [20., 5., 5., 10., 25., 15., 25., 3, "(20., 5.)", "(5., 10.)", "(25., 15.)"]

  $ (cd ../../../../default && demos/demoAssign.exe)
  [[1, 2], [[1, 2], 3], [1, 2], [1, 2], 1, 3, 3, 2, nil, 3]

  $ (cd ../../../../default && demos/demoLists.exe)
  [[[[1], 5], 3], 1, [1, 2, 3], nil]

  $ (cd ../../../../default && demos/demoStringInterp.exe)
  ["str1ng", "10 * 8 = 80", "list=[1, 2, 3]", "", "p1 = (20, 5)"]

  $ (cd ../../../../default && demos/demoPrettyPrinter.exe)
  class Point
    def initialize(x, y)
      @x = x
      @y = y
    end
    
    def x()
      @x
    end
    
    def y()
      @y
    end
    
    def x=(v)
      @x = v
    end
    
    def y=(v)
      @y = v
    end
    
    def Point.distance(p1, p2)
      return sqrt((p1.x() - p2.x()) ** 2 + (p1.y() - p2.y()) ** 2)
    end
    
    def distance_to_origin()
      return sqrt(@x ** 2 + @y ** 2)
    end
    
    def to_s()
      return "(#{@x}, #{@y})"
    end
  end
  
  class AnotherClass
    def initialize(x)
      while x
        while x
          puts(x)
          puts(x)
          puts(x)
          break
        end
      end
    end
  end
  
  origin = Point.new(0, 0)
  
  puts(origin)
  
  p1 = Point.new(20, 5)
  
  p2 = Point.new(5, 4)
  
  def p1.sum(x, y)
    x + y
  end
  
  d = Point.distance(p1, p2)
  
  l = ->(x){x + 1}
  
  x = 2 + 5
  
  y = (2 + 5) * 3
  
  z = (2 + 5) * (3 / 3) ** 10
  
  f = not not not (true or false)
  
  v = (x or y) and (z or f)
  
  k = x or y and z
  0
