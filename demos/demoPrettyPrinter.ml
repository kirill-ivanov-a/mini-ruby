open Ruby_lib.Parser
open Ruby_lib.Pretty_printer
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

class AnotherClass
  def initialize(x)
    while x
     while x
      puts(x)
      puts(x)
      puts(x)
      break
     end
    end
  end
end

origin = Point.new(0, 0)

puts(origin)

p1 = Point.new(20, 5)

p2 = Point.new(5, 4)

def p1.sum(x, y)
   x + y
end

d = Point.distance(p1, p2)

l = ->(x){x + 1}

x = (2 + 5)
y = (2 + 5) * 3
z = (2 + 5) * (3 / 3) ** 10
f = not not not (true or false)
v = (x or y) and (z or f)
k = x or y and z
|}

let () =
  match test with
  | Ok p ->
      let pp_prog_string prog = asprintf "%a" print_prog prog in
      let eq =
        match parse prog @@ pp_prog_string p with
        | Ok prog -> compare prog p
        | Error _ -> -1 in
      print_prog std_formatter p; printf "\n%d" eq
  | Error _ -> printf "syntax error"
