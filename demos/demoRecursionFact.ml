open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
  def fact(n)
    if n < 0
      return nil 
    end
    if n == 1 or n == 0
      return 1 
    end
    return n * fact(n - 1) 
  end
  [fact(0), fact(1), fact(2), fact(3), fact(4), fact(5)]
|}

let () = tester test
