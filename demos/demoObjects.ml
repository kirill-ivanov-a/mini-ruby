open Ruby_lib.Parser
open Ruby_lib.Tester

let test =
  parse prog
    {|
class Point
  @count = 0

  def self.inc_count()
    @count = @count + 1
  end

  def self.count()
    @count
  end

  def initialize(x, y)
    @x = x
    @y = y
    Point.inc_count()
  end

  def x()
    @x
  end

  def y()
    @y
  end

  def x=(v)
    @x = v
  end

  def y=(v)
    @y = v
  end

  def to_s()
    return "(#{@x}, #{@y})"
  end
end

p1 = Point.new(20.0, 5.0)
p2 = Point.new(5.0, 4.0)
p2.y=(10.0)

def p1.coord_sum()
  @x + @y
end

def Point.points_sum(p1, p2)
  return Point.new(p1.x() + p2.x(), p1.y() + p2.y())
end

c_sum = p1.coord_sum()
p3 = Point.points_sum(p1, p2)
[p1.x(), p1.y(), p2.x(), p2.y(), p3.x(), p3.y(), c_sum, Point.count(), p1.to_s(), p2.to_s(), p3.to_s()]
|}

let () = tester test
