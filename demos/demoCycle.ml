open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
x = 20
y = 0
until false
  while true
    if x <= 0 
      break
    end
    if x % 2 == 0
      x = x - 1
      next
    end
    y = y + 1
    x = x - 1
  end
  break
end
y
|}

let () = tester test
