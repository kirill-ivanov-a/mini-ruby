open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
  two = 2
  three_f = 3.0
  three_i = 3
  pow = two ** two ** two ** two
  sum = 5.5 + 10
  sub = 5 - 10.5
  div_i = 10 / 3
  div_f = 10 / three_f
  mul_i = 10 * three_i
  mul_f = 10 * three_f
  mod = 10 % 3
  comb = three_f * three_i / (7 % 4) ** two - 1
  [pow, sum, sub, div_i, div_f, mul_i, mul_f, mod, comb]
|}

let () = tester test
