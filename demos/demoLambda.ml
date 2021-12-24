open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
  compose = ->(f, g, x){ f.call(g.call(x)) }
  f = ->(x){
    ->(x){ x }.call(x) + 1 
  }
  g = lambda {|x| 
  identity = ->(x){ return x }
  return identity.call(x) * identity.call(x)
  }
  x = 4
  compose.call(f, g, x)
|}

let () = tester test
