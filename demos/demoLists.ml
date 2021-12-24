open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
x = [[[1], 2], 3]
y = x[0][0][0]
z = x[0][0]
z = z + [2, 3]
v = x[0]
v[1] = 5
w = x[10]
[x, y, z, w]
|}

let () = tester test
