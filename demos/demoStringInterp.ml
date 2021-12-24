open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
class Point
  def initialize(x, y)
    @x = x
    @y = y
  end
  def to_s()
    return "(#{@x}, #{@y})"
  end
end

p1 = Point.new(20, 5)

str = "str"
ng = "ng"
s1 = "#{str}#{1}#{ng}"
ten = 10
s2 = "10 * 8 = #{ ten * 8 }"
s3 = "list=#{[1, 2, 3]}"
n = nil
s4 = "#{n}"
[s1, s2, s3, s4, "p1 = #{p1.to_s()}"]
|}

let () = tester test
