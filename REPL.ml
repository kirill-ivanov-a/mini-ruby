open Ruby_lib.Parser
open Ruby_lib.Interpreter

let stdlib =
  {|
def fact(n)
  if n < 0
    return nil 
  end
  if n == 1 or n == 0
    return 1 
  end
  return n * fact(n - 1) 
end

def fib(n)
  if n < 0
    return nil 
  end
  if n == 1 or n == 0
    return n 
  end
  return fib(n - 1) + fib(n - 2) 
end

def abs(x)
  if x >= 0
    return x
  else
    return -x
  end
end

def int_part(x)
  if (x > 0)
   return x - (x % 1)
  else
   return x + (x % 1)
  end
end

def floor(x)
  if x % 1 > 0
    return int_part(x) + 1
  else
    if x % 1 == 0
      return x
    else
      return int_part(x)
    end
  end
end

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
|}

let rec repl ctx buffer =
  let str = print_string "(main)> "; read_line () in
  let check_end_of_input s =
    let is_end_of_input = function
      | "" -> false
      | s -> s.[String.length s - 1] |> ( == ) '#' in
    let del_end_of_input s = Str.string_before s (String.length s - 1) in
    if is_end_of_input s then (
      del_end_of_input s |> Buffer.add_string buffer;
      match parse prog (Buffer.contents buffer) with
      | Ok parsed -> (
          let new_ctx = copy_ctx ctx in
          match eval new_ctx parsed with
          | Ok v ->
              print_string "=> ";
              print_value ctx v;
              Buffer.clear buffer;
              repl new_ctx buffer
          | Error msg ->
              print_string "Error: ";
              print_endline msg;
              Buffer.clear buffer;
              repl ctx buffer )
      | Error _ ->
          print_endline "Error: syntax error";
          Buffer.clear buffer;
          repl ctx buffer )
    else Buffer.add_string buffer (str ^ "\n");
    repl ctx buffer in
  check_end_of_input str

let () =
  print_string "------------------// Ruby REPL //------------------\n";
  let ctx = () |> make_exec_ctx in
  let buffer = Buffer.create 1024 in
  let new_ctx = copy_ctx ctx in
  match parse prog stdlib with
  | Ok parsed -> (
    match eval new_ctx parsed with
    | Ok _ -> repl new_ctx buffer
    | Error msg -> print_string "Error: "; print_endline msg; repl ctx buffer )
  | Error _ ->
      print_endline "Failed to load library: syntax error";
      repl ctx buffer
