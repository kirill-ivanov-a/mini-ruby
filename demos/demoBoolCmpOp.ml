open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
  t1 = "str" >= "str" and "str" <= "str" and "str" == "str"
  t2 = (5 + 2) <= 7 and (5 + 2) >= 7 and (5 + 2) == 7 and (not (5 + 2) != 7)
  f1 = [1, 2, 3] != [1, 2, 3]
  f2 = (5 + 2) < 7 or (5 + 2) > 7 or (5 + 2) != 7
  f3 = not ([1, 2, 3] == [1, 2, 3])
  [t1, t2, f1, f2, f3, t1 and t2, t1 and f1 and t2, t1 and f2 or t2, f1 or f2 or f3, not (f1 or f2 or f3), not (t1 and t2), t1 and f1 or t2 and f2 and f3]
|}

let () = tester test
