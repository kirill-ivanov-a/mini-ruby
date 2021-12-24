open Ruby_lib.Ast
open Ruby_lib.Parser
open Format

let test =
  parse prog
    {|
class Point
  def initialize(x, y)
    @x = x
    @y = y
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

  def Point.distance(p1, p2)
    return sqrt((p1.x() - p2.x()) ** 2 + (p1.y() - p2.y()) ** 2)
  end

  def distance_to_origin()
    return sqrt(@x ** 2 + @y ** 2)
  end

  def to_s()
    return "(#{@x}, #{@y})"
  end
end


origin = Point.new(0, 0)
puts(origin)
p1 = Point.new(20.0, 5.0)
p2 = Point.new(5.0, 4.0)
d = Point.distance(p1, p2)

|}

let () =
  match test with
  | Ok prog -> pp_print_list pp_stmt std_formatter prog
  | Error _ -> printf "syntax error"
