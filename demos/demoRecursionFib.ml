open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
  def fib(n)
    if n < 0
      return nil 
    end
    if n == 1 or n == 0
      return n 
    end
    return fib(n - 1) + fib(n - 2) 
  end
  [fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6), fib(7), fib(8), fib(9), fib(10), fib(11)]
|}

let () = tester test
