open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
  l1 = [1, 2, 3]
  l2 = [4, 5]
  l3 = l1 + l2
  l4 = l3 - l2
  l5 = l1 + [4, 5]
  s1 = "str"
  s2 = "ing"
  s3 = s1 + s2
  [l1, l2, l3, l4, l5, s1, s2, s3]
|}

let () = tester test
