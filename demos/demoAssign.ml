open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
def f()
  return [1, 2], 3
end
x1 = [1, 2]
x2 = f()
x3, y1 = f()
x4, y2, z1 = f()
x, y, z = 1, 2, 3, 4
[x1, x2, x3, x4, x, y1, y2, y, z1, z]
|}

let () = tester test
